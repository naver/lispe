#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <regex>
#include <algorithm>
#include <stdexcept>

using Rank = uint32_t;

// Hash for vector<uint8_t>
struct VecU8Hash {
    std::size_t operator()(const std::vector<uint8_t>& v) const {
        std::size_t h = 0;
        for (auto b : v) h = h * 31 + b;
        return h;
    }
};
struct VecU8Equal {
    bool operator()(const std::vector<uint8_t>& a, const std::vector<uint8_t>& b) const {
        return a == b;
    }
};

using Vocab = std::unordered_map<std::vector<uint8_t>, Rank, VecU8Hash, VecU8Equal>;
using Decoder = std::unordered_map<Rank, std::vector<uint8_t>>;
using SpecialTokens = std::unordered_map<std::string, Rank>;
using SpecialDecoder = std::unordered_map<Rank, std::vector<uint8_t>>;

class CoreBPE {
public:
    Vocab encoder;
    SpecialTokens special_tokens_encoder;
    Decoder decoder;
    SpecialDecoder special_tokens_decoder;
    std::regex pattern;
    std::regex special_pattern;
    std::vector<std::vector<uint8_t>> sorted_token_bytes;

    CoreBPE(const Vocab& enc,
            const SpecialTokens& special_enc,
            const std::string& regex_pattern)
        : encoder(enc), special_tokens_encoder(special_enc),
          pattern(regex_pattern)
    {
        // Build decoder
        for (const auto& kv : encoder)
            decoder[kv.second] = kv.first;
        for (const auto& kv : special_tokens_encoder)
            special_tokens_decoder[kv.second] = std::vector<uint8_t>(kv.first.begin(), kv.first.end());

        // Build sorted_token_bytes
        for (const auto& kv : encoder)
            sorted_token_bytes.push_back(kv.first);
        std::sort(sorted_token_bytes.begin(), sorted_token_bytes.end());

        // Build special regex
        std::string special_regex_str;
        for (const auto& kv : special_tokens_encoder) {
            if (!special_regex_str.empty()) special_regex_str += "|";
            special_regex_str += std::regex_replace(kv.first, std::regex(R"([-[\]{}()*+?.,\^$|#\s])"), R"(\$&)");
        }
        if (!special_regex_str.empty())
            special_pattern = std::regex(special_regex_str);
    }

    // BPE merge
    std::vector<std::pair<size_t, Rank>> byte_pair_merge(const std::vector<uint8_t>& piece) const {
        std::vector<std::pair<size_t, Rank>> parts;
        if (piece.size() < 2) {
            parts.push_back({0, Rank(-1)});
            parts.push_back({piece.size(), Rank(-1)});
            return parts;
        }
        Rank min_rank = Rank(-1);
        size_t min_pos = size_t(-1);
        for (size_t i = 0; i < piece.size() - 1; ++i) {
            std::vector<uint8_t> pair = {piece[i], piece[i+1]};
            Rank rank = encoder.count(pair) ? encoder.at(pair) : Rank(-1);
            if (rank < min_rank) {
                min_rank = rank;
                min_pos = i;
            }
            parts.push_back({i, rank});
        }
        parts.push_back({piece.size() - 1, Rank(-1)});
        parts.push_back({piece.size(), Rank(-1)});

        auto get_rank = [&](const std::vector<std::pair<size_t, Rank>>& parts, size_t i) -> Rank {
            if (i + 3 < parts.size()) {
                std::vector<uint8_t> sub(piece.begin() + parts[i].first, piece.begin() + parts[i+3].first);
                return encoder.count(sub) ? encoder.at(sub) : Rank(-1);
            }
            return Rank(-1);
        };

        while (min_rank != Rank(-1)) {
            size_t i = min_pos;
            if (i > 0) parts[i-1].second = get_rank(parts, i-1);
            parts[i].second = get_rank(parts, i);
            parts.erase(parts.begin() + i + 1);

            min_rank = Rank(-1);
            min_pos = size_t(-1);
            for (size_t j = 0; j < parts.size() - 1; ++j) {
                if (parts[j].second < min_rank) {
                    min_rank = parts[j].second;
                    min_pos = j;
                }
            }
        }
        return parts;
    }

    // BPE encode
    std::vector<Rank> byte_pair_encode(const std::vector<uint8_t>& piece) const {
        if (piece.size() == 1) {
            return {encoder.at(piece)};
        }
        auto parts = byte_pair_merge(piece);
        std::vector<Rank> result;
        for (size_t i = 0; i + 1 < parts.size(); ++i) {
            std::vector<uint8_t> sub(piece.begin() + parts[i].first, piece.begin() + parts[i+1].first);
            result.push_back(encoder.at(sub));
        }
        return result;
    }

    // BPE split
    std::vector<std::vector<uint8_t>> byte_pair_split(const std::vector<uint8_t>& piece) const {
        auto parts = byte_pair_merge(piece);
        std::vector<std::vector<uint8_t>> result;
        for (size_t i = 0; i + 1 < parts.size(); ++i) {
            result.emplace_back(piece.begin() + parts[i].first, piece.begin() + parts[i+1].first);
        }
        return result;
    }

    // Encode ordinary (sans tokens spéciaux)
    std::vector<Rank> encode_ordinary(const std::string& text) const {
        std::vector<Rank> ret;
        auto words_begin = std::sregex_iterator(text.begin(), text.end(), pattern);
        auto words_end = std::sregex_iterator();
        for (auto it = words_begin; it != words_end; ++it) {
            std::string piece = it->str();
            std::vector<uint8_t> bytes(piece.begin(), piece.end());
            auto found = encoder.find(bytes);
            if (found != encoder.end()) {
                ret.push_back(found->second);
            } else {
                auto tokens = byte_pair_encode(bytes);
                ret.insert(ret.end(), tokens.begin(), tokens.end());
            }
        }
        return ret;
    }

    // Decode
    std::string decode(const std::vector<Rank>& tokens) const {
        std::string result;
        for (auto token : tokens) {
            auto it = decoder.find(token);
            if (it != decoder.end()) {
                result.append(it->second.begin(), it->second.end());
            } else {
                auto it2 = special_tokens_decoder.find(token);
                if (it2 != special_tokens_decoder.end()) {
                    result.append(it2->second.begin(), it2->second.end());
                } else {
                    throw std::runtime_error("Unknown token in decode: " + std::to_string(token));
                }
            }
        }
        return result;
    }
};

// --- Exemple d'utilisation ---
int main() {
    // Exemple de vocabulaire et tokens spéciaux
    Vocab vocab;
    vocab[{uint8_t('a'), uint8_t('b')}] = 0;
    vocab[{uint8_t('c'), uint8_t('d')}] = 1;
    vocab[{uint8_t('a')}] = 2;
    vocab[{uint8_t('b')}] = 3;
    vocab[{uint8_t('c')}] = 4;
    vocab[{uint8_t('d')}] = 5;

    SpecialTokens specials;
    specials["<s>"] = 100;
    specials["</s>"] = 101;

    // Regex pour découper en mots (à adapter selon le tokenizer réel)
    std::string pattern = R"(\w+|\s+|[^\w\s]+)";

    CoreBPE bpe(vocab, specials, pattern);

    std::string text = "abcd";
    auto encoded = bpe.encode_ordinary(text);
    std::cout << "Encoded: ";
    for (auto id : encoded) std::cout << id << " ";
    std::cout << std::endl;

    std::string decoded = bpe.decode(encoded);
    std::cout << "Decoded: " << decoded << std::endl;

    return 0;
}
