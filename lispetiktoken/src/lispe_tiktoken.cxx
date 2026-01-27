/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//
//  lispe_tiktoken.cxx


/*
 This the template for new 'extensions' in lispe
 */

 /*
 On MAC OS, install: brew install icu4c
 To have access to icu::Regex
 */

#include "lispe.h"
#include "lispe_tiktoken.h"

#ifdef WIN32
#if (_MSC_VER >= 1900)
#pragma comment(lib, "legacy_stdio_definitions.lib")
FILE _iob[] = { *stdin, *stdout, *stderr };
extern "C" FILE * __cdecl __iob_func(void) { return _iob; }
#endif
#endif

#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <unicode/regex.h>
#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <algorithm>
#include <stdexcept>

static int16_t corebpe_type = 0;

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

// Méta-caractères couramment utilisés dans les tokenizers BPE
static uchar meta_space[] = {0xC4, 0xA0, 0};        // Ġ - espace en début de token
static uchar meta_newline[] = {0xC4, 0x8A, 0};     // Ċ - nouvelle ligne
static uchar meta_tab[] = {0xC4, 0x89, 0};         // ĉ - tabulation
static uchar meta_excl[] = {0xC4, 0x81, 0};        // Ā - point d'exclamation
static uchar meta_quest[] = {0xC4, 0x9F, 0};       // ğ - point d'interrogation
static uchar meta_comma[] = {0xC4, 0x8B, 0};       // ċ - virgule
static uchar meta_period[] = {0xC4, 0xA3, 0};      // ģ - point
static uchar meta_colon[] = {0xC4, 0xB7, 0};       // ķ - deux points
static uchar meta_semicol[] = {0xC4, 0xBB, 0};     // Ļ - point-virgule
static uchar meta_quote[] = {0xC4, 0x9D, 0};       // ĝ - guillemet
static uchar meta_dquote[] = {0xC4, 0x9C, 0};      // Ĝ - double guillemet
static uchar meta_lparen[] = {0xC4, 0x84, 0};      // Ą - parenthèse ouvrante
static uchar meta_rparen[] = {0xC4, 0x85, 0};      // ą - parenthèse fermante
static uchar meta_lbracket[] = {0xC4, 0x90, 0};    // Đ - crochet ouvrant
static uchar meta_rbracket[] = {0xC4, 0x91, 0};    // đ - crochet fermant
static uchar meta_lbrace[] = {0xC4, 0x9A, 0};      // Ě - accolade ouvrante
static uchar meta_rbrace[] = {0xC4, 0x9B, 0};      // ě - accolade fermante
static uchar meta_hyphen[] = {0xC4, 0x8F, 0};      // Ď - tiret
static uchar meta_underscore[] = {0xC4, 0x8D, 0};  // č - underscore
static uchar meta_slash[] = {0xC4, 0x8C, 0};       // Č - slash
static uchar meta_backslash[] = {0xC4, 0x92, 0};   // Ē - backslash
static uchar meta_pipe[] = {0xC4, 0x96, 0};        // Ė - pipe |
static uchar meta_ampersand[] = {0xC4, 0x86, 0};   // Ć - esperluette &
static uchar meta_at[] = {0xC4, 0x80, 0};          // Ā - arobase @
static uchar meta_hash[] = {0xC4, 0x82, 0};        // Ă - dièse #
static uchar meta_dollar[] = {0xC4, 0x88, 0};      // Ĉ - dollar $
static uchar meta_percent[] = {0xC4, 0x94, 0};     // Ĕ - pourcentage %
static uchar meta_caret[] = {0xC4, 0x95, 0};       // ĕ - circonflexe ^
static uchar meta_asterisk[] = {0xC4, 0x87, 0};    // ć - astérisque *
static uchar meta_plus[] = {0xC4, 0x8E, 0};        // Ď - plus +
static uchar meta_equal[] = {0xC4, 0x93, 0};       // ē - égal =
static uchar meta_less[] = {0xC4, 0x97, 0};        // ė - inférieur <
static uchar meta_greater[] = {0xC4, 0x99, 0};     // ę - supérieur >
static uchar meta_tilde[] = {0xC4, 0xB9, 0};       // ĺ - tilde ~
static uchar meta_backtick[] = {0xC4, 0x98, 0};    // Ę - backtick `

// Méta-caractères pour les caractères accentués couramment mal encodés
static uchar meta_ae[] = {0xC3, 0xA6, 0};          // æ
static uchar meta_a_circumflex[] = {0xC3, 0xA2, 0}; // â
static uchar meta_a_grave[] = {0xC3, 0xA0, 0};     // à
static uchar meta_a_acute[] = {0xC3, 0xA1, 0};     // á
static uchar meta_e_circumflex[] = {0xC3, 0xAA, 0}; // ê
static uchar meta_e_grave[] = {0xC3, 0xA8, 0};     // è
static uchar meta_e_acute[] = {0xC3, 0xA9, 0};     // é
static uchar meta_i_circumflex[] = {0xC3, 0xAE, 0}; // î
static uchar meta_o_circumflex[] = {0xC3, 0xB4, 0}; // ô
static uchar meta_u_circumflex[] = {0xC3, 0xBB, 0}; // û
static uchar meta_c_cedilla[] = {0xC3, 0xA7, 0};   // ç
static uchar meta_n_tilde[] = {0xC3, 0xB1, 0};     // ñ

// Caractères spéciaux avec cédille/accent couramment utilisés dans les tokenizers
static uchar meta_g_cedilla[] = {0xC4, 0xA2, 0};   // Ģ - G avec cédille
static uchar meta_l_slash[] = {0xC5, 0x82, 0};     // ł - l barré
static uchar meta_s_acute[] = {0xC5, 0x9B, 0};     // ś - s aigu
static uchar meta_z_acute[] = {0xC5, 0xBA, 0};     // ź - z aigu
static uchar meta_z_dot[] = {0xC5, 0xBC, 0};       // ż - z point
static uchar meta_c_acute[] = {0xC4, 0x87, 0};     // ć - c aigu
static uchar meta_n_acute[] = {0xC5, 0x84, 0};     // ń - n aigu
static uchar meta_a_ogonek[] = {0xC4, 0x85, 0};    // ą - a ogonek
static uchar meta_e_ogonek[] = {0xC4, 0x99, 0};    // ę - e ogonek
static uchar meta_apostrophe[] = {0xE2, 0xB0, 0x99, 0}; 

// Autres caractères spéciaux de tokenizers
static uchar meta_i_dot[] = {0xC4, 0xB1, 0};       // ı - i sans point
static uchar meta_o_slash[] = {0xC3, 0xB8, 0};     // ø - o barré
static uchar meta_d_stroke[] = {0xC4, 0x91, 0};    // đ - d barré
static uchar meta_h_stroke[] = {0xC4, 0xA7, 0};    // ħ - h barré
static uchar meta_t_stroke[] = {0xC5, 0xA7, 0};    // ŧ - t barré

// Patterns de contractions couramment encodés dans les tokenizers GPT
static uchar meta_contraction_s[] = {0xC4, 0xA2, 0x3B, 0}; // Ģ; = 's
static uchar meta_contraction_t[] = {0xC4, 0xA4, 0};       // Ĥ = 't  
static uchar meta_contraction_ll[] = {0xC4, 0xBB, 0};      // Ļ = 'll
static uchar meta_contraction_re[] = {0xC4, 0xBA, 0};      // ĺ = 're
static uchar meta_contraction_ve[] = {0xC4, 0xB5, 0xC5, 0x8B, 0}; // ĵŋ = 've

// Caractères spéciaux Unicode couramment utilisés
static uchar meta_ellipsis[] = {0xE2, 0x80, 0xA6, 0}; // … (U+2026)
static uchar meta_mdash[] = {0xE2, 0x80, 0x94, 0};    // — (U+2014)
static uchar meta_ndash[] = {0xE2, 0x80, 0x93, 0};    // – (U+2013)
static uchar meta_lsquo[] = {0xE2, 0x80, 0x98, 0};    // ' (U+2018)
static uchar meta_rsquo[] = {0xE2, 0x80, 0x99, 0};    // ' (U+2019)
static uchar meta_ldquo[] = {0xE2, 0x80, 0x9C, 0};    // " (U+201C)
static uchar meta_rdquo[] = {0xE2, 0x80, 0x9D, 0};    // " (U+201D)

class CoreBPE {
public:
    Vocab encoder;
    SpecialTokens special_tokens_encoder;
    Decoder decoder;
    SpecialDecoder special_tokens_decoder;
    icu::RegexPattern* pattern;
    icu::RegexPattern* special_pattern;
    std::vector<std::vector<uint8_t>> sorted_token_bytes;
    std::string espace;
    std::unordered_map<std::string, std::string> meta_characters;

    CoreBPE(const Vocab& enc,
            const SpecialTokens& special_enc,
            const std::string& regex_pattern_utf8)
        : encoder(enc), special_tokens_encoder(special_enc),
          pattern(nullptr), special_pattern(nullptr), espace((char*)meta_space)
    {
        // Initialize default meta-characters mappings
        // Caractères de contrôle de base
        meta_characters[(char*)meta_space] = " ";
#ifdef WIN32        
        meta_characters[(char*)meta_newline] = "\r\n";
#else
        meta_characters[(char*)meta_newline] = "\n";        
#endif
        meta_characters[(char*)meta_tab] = "\t";
        
        // Ponctuation
        meta_characters[(char*)meta_excl] = "!";
        meta_characters[(char*)meta_quest] = "?";
        meta_characters[(char*)meta_comma] = ",";
        meta_characters[(char*)meta_period] = ".";
        meta_characters[(char*)meta_colon] = ":";
        meta_characters[(char*)meta_semicol] = ";";
        meta_characters[(char*)meta_quote] = "'";
        meta_characters[(char*)meta_dquote] = "\"";
        
        // Parenthèses et crochets
        meta_characters[(char*)meta_lparen] = "(";
        meta_characters[(char*)meta_rparen] = ")";
        meta_characters[(char*)meta_lbracket] = "[";
        meta_characters[(char*)meta_rbracket] = "]";
        meta_characters[(char*)meta_lbrace] = "{";
        meta_characters[(char*)meta_rbrace] = "}";
        
        // Symboles courants
        meta_characters[(char*)meta_hyphen] = "-";
        meta_characters[(char*)meta_underscore] = "_";
        meta_characters[(char*)meta_slash] = "/";
        meta_characters[(char*)meta_backslash] = "\\";
        meta_characters[(char*)meta_pipe] = "|";
        meta_characters[(char*)meta_ampersand] = "&";
        meta_characters[(char*)meta_at] = "@";
        meta_characters[(char*)meta_hash] = "#";
        meta_characters[(char*)meta_dollar] = "$";
        meta_characters[(char*)meta_percent] = "%";
        meta_characters[(char*)meta_caret] = "^";
        meta_characters[(char*)meta_asterisk] = "*";
        meta_characters[(char*)meta_plus] = "+";
        meta_characters[(char*)meta_equal] = "=";
        meta_characters[(char*)meta_less] = "<";
        meta_characters[(char*)meta_greater] = ">";
        meta_characters[(char*)meta_tilde] = "~";
        meta_characters[(char*)meta_backtick] = "`";
        
        // Caractères accentués français
        meta_characters[(char*)meta_ae] = "æ";
        meta_characters[(char*)meta_a_circumflex] = "â";
        meta_characters[(char*)meta_a_grave] = "à";
        meta_characters[(char*)meta_a_acute] = "á";
        meta_characters[(char*)meta_e_circumflex] = "ê";
        meta_characters[(char*)meta_e_grave] = "è";
        meta_characters[(char*)meta_e_acute] = "é";
        meta_characters[(char*)meta_i_circumflex] = "î";
        meta_characters[(char*)meta_o_circumflex] = "ô";
        meta_characters[(char*)meta_u_circumflex] = "û";
        meta_characters[(char*)meta_c_cedilla] = "ç";
        meta_characters[(char*)meta_n_tilde] = "ñ";
        
        // Caractères spéciaux avec cédille/accent (couramment utilisés dans les tokenizers)
        meta_characters[(char*)meta_apostrophe] = "’";
        meta_characters[(char*)meta_g_cedilla] = "'"; // Ģ souvent utilisé pour représenter l'apostrophe dans les contractions
        meta_characters[(char*)meta_l_slash] = "l";
        meta_characters[(char*)meta_s_acute] = "s";
        meta_characters[(char*)meta_z_acute] = "z";
        meta_characters[(char*)meta_z_dot] = "z";
        meta_characters[(char*)meta_c_acute] = "c";
        meta_characters[(char*)meta_n_acute] = "n";
        meta_characters[(char*)meta_a_ogonek] = "a";
        meta_characters[(char*)meta_e_ogonek] = "e";
        
        // Autres caractères spéciaux
        meta_characters[(char*)meta_i_dot] = "i";
        meta_characters[(char*)meta_o_slash] = "o";
        meta_characters[(char*)meta_d_stroke] = "d";
        meta_characters[(char*)meta_h_stroke] = "h";
        meta_characters[(char*)meta_t_stroke] = "t";
        
        // Patterns de contractions couramment encodés
        meta_characters[(char*)meta_contraction_s] = "'s";   // Ģ; = 's (That's)
        meta_characters[(char*)meta_contraction_t] = "'t";   // Ĥ = 't (won't, can't)
        meta_characters[(char*)meta_contraction_ll] = "'ll"; // Ļ = 'll (I'll, you'll)
        meta_characters[(char*)meta_contraction_re] = "'re"; // ĺ = 're (you're, we're)
        meta_characters[(char*)meta_contraction_ve] = "'ve"; // ĵŋ = 've (I've, we've)
        
        // Caractères Unicode spéciaux
        meta_characters[(char*)meta_ellipsis] = "...";
        meta_characters[(char*)meta_mdash] = "—";
        meta_characters[(char*)meta_ndash] = "–";
        meta_characters[(char*)meta_lsquo] = "'";
        meta_characters[(char*)meta_rsquo] = "'";
        meta_characters[(char*)meta_ldquo] = "\"";
        meta_characters[(char*)meta_rdquo] = "\"";
        
        // Build decoder
        for (const auto& kv : encoder)
            decoder[kv.second] = kv.first;
        for (const auto& kv : special_tokens_encoder)
            special_tokens_decoder[kv.second] = std::vector<uint8_t>(kv.first.begin(), kv.first.end());

        // Build sorted_token_bytes
        for (const auto& kv : encoder)
            sorted_token_bytes.push_back(kv.first);
        std::sort(sorted_token_bytes.begin(), sorted_token_bytes.end());

        // Compile ICU regex for pattern
        UErrorCode status = U_ZERO_ERROR;
        icu::UnicodeString pattern_ustr = icu::UnicodeString::fromUTF8(regex_pattern_utf8);
        pattern = icu::RegexPattern::compile(pattern_ustr, 0, status);
        if (U_FAILURE(status)) {
            throw new Error("Error compiling regex ICU: " + std::string(u_errorName(status)));
        }

        // Build special regex (optional, not used in encode_ordinary)
        // You can implement special_pattern if needed for special token matching
        special_pattern = nullptr;
    }

    ~CoreBPE() {
        if (pattern) delete pattern;
        if (special_pattern) delete special_pattern;
    }

    long get_special_encoding(std::string& s) {
        if (special_tokens_encoder.find(s) != special_tokens_encoder.end())
            return special_tokens_encoder[s];
        return -1;
    }

    // Retourne la taille du vocabulaire
    long get_vocab_size() const {
        return encoder.size();
    }

    // ====================
    // Decoding methods (equivalent to Rust implementation)
    // ====================

    // Decode tokens to bytes (equivalent to decode_bytes in Rust)
    std::vector<uint8_t> decode_bytes(const std::vector<Rank>& tokens) const {
        std::vector<uint8_t> result;
        for (Rank token : tokens) {
            auto it = decoder.find(token);
            if (it != decoder.end()) {
                const auto& token_bytes = it->second;
                result.insert(result.end(), token_bytes.begin(), token_bytes.end());
            } else {
                auto special_it = special_tokens_decoder.find(token);
                if (special_it != special_tokens_decoder.end()) {
                    const auto& token_bytes = special_it->second;
                    result.insert(result.end(), token_bytes.begin(), token_bytes.end());
                } else {
                    throw new Error("Token " + std::to_string(token) + " not found in decoder");
                }
            }
        }
        return result;
    }

    // Decode single token to bytes (equivalent to decode_single_token_bytes in Rust)
    std::vector<uint8_t> decode_single_token_bytes(Rank token) const {
        auto it = decoder.find(token);
        if (it != decoder.end()) {
            return it->second;
        }
        auto special_it = special_tokens_decoder.find(token);
        if (special_it != special_tokens_decoder.end()) {
            return special_it->second;
        }
        throw new Error("Token " + std::to_string(token) + " not found in decoder");
    }

    // Get all token byte values (equivalent to token_byte_values in Rust)
    std::vector<std::vector<uint8_t>> get_token_byte_values() const {
        return sorted_token_bytes;
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
            auto it = encoder.find(piece);
            if (it != encoder.end()) {
                return {it->second};
            } else {
                // Token inconnu, on retourne un id spécial ou on lève une erreur
                // Ici, on retourne un vecteur vide
                return {};
            }
        }
        auto parts = byte_pair_merge(piece);
        std::vector<Rank> result;
        for (size_t i = 0; i + 1 < parts.size(); ++i) {
            std::vector<uint8_t> sub(piece.begin() + parts[i].first, piece.begin() + parts[i+1].first);
            auto it = encoder.find(sub);
            if (it != encoder.end()) {
                result.push_back(it->second);
            } else {
                // Token inconnu, on ignore ou on gère différemment
                // Ici, on ne fait rien (on pourrait lever une erreur ou ajouter un id spécial)
            }
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
    std::vector<Rank> encode_ordinary(const std::string& text_utf8) const {
        std::vector<Rank> ret;
        UErrorCode status = U_ZERO_ERROR;
        icu::UnicodeString utext = icu::UnicodeString::fromUTF8(text_utf8);
        std::unique_ptr<icu::RegexMatcher> matcher(pattern->matcher(utext, status));
        if (U_FAILURE(status)) {
            throw new Error("Error: RegexMatcher ICU Creation: " + std::string(u_errorName(status)));
        }
        while (matcher->find(status) && U_SUCCESS(status)) {
            icu::UnicodeString piece = matcher->group(status);
            std::string piece_utf8;
            piece.toUTF8String(piece_utf8);
            std::vector<uint8_t> bytes(piece_utf8.begin(), piece_utf8.end());
            if (!bytes.empty()) {
                if (bytes[0] == 0x20) {
                    // Remplacer l'espace initial par les bytes UTF-8 de U+0120 (0xC4 0xA0)
                    bytes.erase(bytes.begin());
                    bytes.insert(bytes.begin(), meta_space[1]);
                    bytes.insert(bytes.begin(), meta_space[0]);
                }
                if (bytes[0] == 0x10 || bytes[0] == 0x0A) {
                    // Remplacer l'espace initial par les bytes UTF-8 de U+0120 (0xC4 0xA0)
                    bytes.erase(bytes.begin());
                    bytes.insert(bytes.begin(), meta_newline[1]);
                    bytes.insert(bytes.begin(), meta_newline[0]);
                }
            }
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
    u_ustring decode(const std::vector<Rank>& tokens) const {        
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
                    throw new Error("Unknown token in decode: " + std::to_string(token));
                }
            }
        }

        for (const auto& k: meta_characters) {
            if (result.find(k.first) != -1)
                result = s_replacingstring(result, k.first, k.second);
        }
        
        u_ustring final_str;
        s_utf8_to_unicode(final_str, result, result.size());
        return final_str;
    }
};

class LispE_CoreBPE : public Element {
public:
    CoreBPE* bpe;
    
    LispE_CoreBPE(LispE* lisp) : Element(corebpe_type) {
        bpe = NULL;
        Element* vocab = lisp->get_variable(L"vocab");
        Element* special = lisp->get_variable(L"special_tokens");
        Element* pattern = lisp->get_variable(L"pattern");
        if (vocab->type != t_dictionary)
            throw new Error("Wrong vocabulary format for tiktoken");
        Vocab vocabulary;
        string s;
        std::vector<uint8_t> v;

        long i;
        u_ustring key;
        for (const auto& k : ((Dictionary*)vocab)->dictionary) {
            key = k.first;
            s = "";
            s_unicode_to_utf8(s, key);
            v.clear();
            for (i = 0; i < s.size(); i++)
                v.push_back(s[i]);
            vocabulary[v] = k.second->asInteger();
        }

        if (!special->isList())
            throw new Error("Wrong special tokens format for tiktoken");
        
        SpecialTokens specialtokens;
        Element* content;
        Element* id;
        Element* d;
        for (i = 0; i < special->size(); i++) {
            d = special->index(i);
            if (!d->isDictionary())
                throw new Error("Wrong special tokens format for tiktoken: elements should be dictionaries");
            key = U"content";
            content = d->protected_index(lisp, key);
            key = U"id";
            id = d->protected_index(lisp, key);
            s = content->toString(lisp);
            specialtokens[s] = id->asInteger();
        }

        s = pattern->toString(lisp);
        bpe = new CoreBPE(vocabulary, specialtokens, s);
    }

    Element* set_space(LispE* lisp, std::string espace) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        bpe->espace = espace;
        bpe->meta_characters[espace] = " ";
        return True_;
    }

    Element* add_meta_character(LispE* lisp, std::string meta_char, std::string replacement) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        bpe->meta_characters[meta_char] = replacement;
        return True_;
    }

    Element* remove_meta_character(LispE* lisp, std::string meta_char) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        auto it = bpe->meta_characters.find(meta_char);
        if (it != bpe->meta_characters.end()) {
            bpe->meta_characters.erase(it);
        }
        return True_;
    }

    Element* list_meta_characters(LispE* lisp) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        Element* metas = lisp->get_variable("metas");
        u_ustring key;
        string s;
        if (metas == null_) {
            Dictionary* result = lisp->provideDictionary();
            for (const auto& meta : bpe->meta_characters) {
                s = meta.first;
                key = U"";
                s_utf8_to_unicode(key, s, s.size());
                s = meta.second;
                result->recording(key, lisp->provideString(s));
            }
            return result;
        }
        if (!metas->isDictionary())
            throw new Error("Error: Expecting a dictionary as input");

        Dictionary* liste = (Dictionary*)metas;
        for (const auto& k : liste->dictionary) {
            s = "";
            key = k.first;
            s_unicode_to_utf8(s, key);
            bpe->meta_characters[s] = k.second->toString(lisp);
        }
        return True_;
    }

    Element* decode(LispE *lisp, Element *ids)
    {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        std::vector<Rank> tokens;
        if (ids->type == t_integers) {
            Integers* idis = (Integers*)ids;
            for (long i = 0; i < idis->size(); i++) {
                tokens.push_back((Rank)idis->liste[i]);
            }
        }
        else {
            for (long i = 0; i < ids->size(); ++i)
                tokens.push_back(ids->index(i)->asInteger());
        }
        
        u_ustring result = bpe->decode(tokens);
        return lisp->provideString(result);
    }

    Element* encode(LispE *lisp, Element *texte)
    {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        std::string s = texte->toString(lisp);
        std::vector<Rank> ids = bpe->encode_ordinary(s);
        // On retourne une liste LispE d'entiers
        Integers *result = lisp->provideIntegers();
        for (auto id : ids)
            result->liste.push_back(id);
        return result;
    }

    Element* special_encoding(LispE* lisp, Element* text) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        std::string s = text->toString(lisp);
        return lisp->provideInteger(bpe->get_special_encoding(s));
    }

    Element* vocab_size(LispE* lisp) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        return lisp->provideInteger(bpe->get_vocab_size());
    }

    // ====================
    // New decoding methods
    // ====================

    Element* decode_bytes(LispE* lisp, Element* ids) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        std::vector<Rank> tokens;
        if (ids->type == t_integers) {
            Integers* idis = (Integers*)ids;
            for (long i = 0; i < idis->size(); i++) {
                tokens.push_back((Rank)idis->liste[i]);
            }
        }
        else {
            for (long i = 0; i < ids->size(); ++i)
                tokens.push_back(ids->index(i)->asInteger());
        }
        
        try {
            std::vector<uint8_t> result_bytes = bpe->decode_bytes(tokens);
            // Convertir les bytes en liste d'entiers LispE
            Integers* result = lisp->provideIntegers();
            for (uint8_t byte : result_bytes) {
                result->liste.push_back(byte);
            }
            return result;
        } catch (Error* e) {
            throw e;
        }
    }

    Element* decode_single_token_bytes(LispE* lisp, Element* token) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        
        Rank tok = token->asInteger();
        try {
            std::vector<uint8_t> result_bytes = bpe->decode_single_token_bytes(tok);
            // Convertir les bytes en liste d'entiers LispE
            Integers* result = lisp->provideIntegers();
            for (uint8_t byte : result_bytes) {
                result->liste.push_back(byte);
            }
            return result;
        } catch (Error* e) {
            throw e;
        }
    }

    Element* get_token_byte_values(LispE* lisp) {
        if (bpe == NULL)
            throw new Error("BPE object not initialized");
        
        std::vector<std::vector<uint8_t>> token_bytes = bpe->get_token_byte_values();
        // Convertir en liste de listes d'entiers LispE
        List* result = lisp->provideList();
        for (const auto& bytes : token_bytes) {
            Integers* byte_list = lisp->provideIntegers();
            for (uint8_t byte : bytes) {
                byte_list->liste.push_back(byte);
            }
            result->append(byte_list);
        }
        return result;
    }

    ~LispE_CoreBPE() {
        if (bpe != NULL)
            delete bpe;
    }
};

Element* Lispe_tiktoken::eval(LispE* lisp) {
    //The name defined in the extension is not insignificant, it is used to retrieve our arguments.
    switch (action) {
        case corebpe_create: {
            return new LispE_CoreBPE(lisp);
        }
        case corebpe_encode: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->encode(lisp, lisp->get_variable("text"));
        }
        case corebpe_decode: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->decode(lisp, lisp->get_variable("ids"));
        }
        case corebpe_special_encode: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->special_encoding(lisp, lisp->get_variable("text"));            
        }
        case corebpe_vocab_size: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->vocab_size(lisp);
        }
        case corebpe_set_space: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->set_space(lisp, lisp->get_variable(U"str")->toString(lisp));
        }
        case corebpe_add_meta: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->add_meta_character(lisp, 
                lisp->get_variable(U"meta_char")->toString(lisp),
                lisp->get_variable(U"replacement")->toString(lisp));
        }
        case corebpe_remove_meta: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->remove_meta_character(lisp, 
                lisp->get_variable(U"meta_char")->toString(lisp));
        }
        case corebpe_list_meta: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->list_meta_characters(lisp);
        }
        case corebpe_decode_bytes: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->decode_bytes(lisp, lisp->get_variable("ids"));
        }
        case corebpe_decode_single_token_bytes: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->decode_single_token_bytes(lisp, lisp->get_variable("token"));
        }
        case corebpe_token_byte_values: {
            Element* e = lisp->get_variable("bpe_object");
            if (e->type != corebpe_type)
                throw new Error("Error: expecting a `tiktoken_' type");
            return ((LispE_CoreBPE*)e)->get_token_byte_values(lisp);
        }
    }
}

//We use this instruction to return a description of the instruction
//Indeed, just do: (print tiktoken_example) to get this information
wstring Lispe_tiktoken::asString(LispE* lisp) {
    switch (action) {
        case corebpe_create:
            return L"Create a tiktoken object with vocab and special tokens";
        case corebpe_encode:
            return L"Use tiktoken object to encode a string";
        case corebpe_decode:
            return L"Use tiktoken object to decode a list of integers";
        case corebpe_special_encode:
            return L"Return the id of a special characters";
        case corebpe_vocab_size:
            return L"Return the vocabulary size of the tokenizer";
        case corebpe_set_space:
            return L"Set the space character";
        case corebpe_add_meta:
            return L"Add a meta-character mapping for decoding";
        case corebpe_remove_meta:
            return L"Remove a meta-character mapping";
        case corebpe_list_meta:
            return L"List all meta-character mappings";
        case corebpe_decode_bytes:
            return L"Decode tokens to raw bytes";
        case corebpe_decode_single_token_bytes:
            return L"Decode a single token to raw bytes";
        case corebpe_token_byte_values:
            return L"Get all token byte values from vocabulary";
        default:
            return L"Unknown command";
    }
}

extern "C" {
Exporting bool InitialisationModule(LispE* lisp) {
    corebpe_type = lisp->provideAtom(U"tiktoken_")->label();
    //We first create the body of the function
    lisp->extension("deflib tiktoken_create (vocab special_tokens pattern)", new Lispe_tiktoken(corebpe_create));
    lisp->extension("deflib tiktoken_encode (bpe_object text)", new Lispe_tiktoken(corebpe_encode));
    lisp->extension("deflib tiktoken_decode (bpe_object ids)", new Lispe_tiktoken(corebpe_decode));
    lisp->extension("deflib tiktoken_special_encode (bpe_object text)", new Lispe_tiktoken(corebpe_special_encode));
    lisp->extension("deflib tiktoken_vocab_size (bpe_object)", new Lispe_tiktoken(corebpe_vocab_size));
    lisp->extension("deflib tiktoken_set_space (bpe_object str)", new Lispe_tiktoken(corebpe_set_space));
    lisp->extension("deflib tiktoken_add_meta (bpe_object meta_char replacement)", new Lispe_tiktoken(corebpe_add_meta));
    lisp->extension("deflib tiktoken_remove_meta (bpe_object meta_char)", new Lispe_tiktoken(corebpe_remove_meta));
    lisp->extension("deflib tiktoken_list_meta (bpe_object (metas))", new Lispe_tiktoken(corebpe_list_meta));
    lisp->extension("deflib tiktoken_decode_bytes (bpe_object ids)", new Lispe_tiktoken(corebpe_decode_bytes));
    lisp->extension("deflib tiktoken_decode_single_token_bytes (bpe_object token)", new Lispe_tiktoken(corebpe_decode_single_token_bytes));
    lisp->extension("deflib tiktoken_token_byte_values (bpe_object)", new Lispe_tiktoken(corebpe_token_byte_values));
    return true;
}
}


