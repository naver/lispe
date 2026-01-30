#ifndef TENSOR_SVD_H
#define TENSOR_SVD_H

#include <torch/torch.h>
#include <vector>
#include <algorithm>
#include <iostream>
#include <chrono>

template<typename T = float>
class TensorSVD {
private:
    torch::Tensor U, V, S;
    int m, n;
    torch::Device device;
    torch::ScalarType dtype;
    
    // Convertir le type C++ en ScalarType torch
    static torch::ScalarType getScalarType() {
        if constexpr (std::is_same_v<T, float>) {
            return torch::kFloat32;
        } else if constexpr (std::is_same_v<T, double>) {
            return torch::kFloat64;
        } else if constexpr (std::is_same_v<T, c10::Half>) {
            return torch::kFloat16;
        } else if constexpr (std::is_same_v<T, c10::BFloat16>) {
            return torch::kBFloat16;
        } else {
            static_assert(std::is_same_v<T, float> || std::is_same_v<T, double> || 
                         std::is_same_v<T, c10::Half> || std::is_same_v<T, c10::BFloat16>, 
                         "Type non supporté : utilisez float, double, c10::Half ou c10::BFloat16");
            return torch::kFloat32; // fallback
        }
    }
    
    // Algorithme de puissance pour trouver la valeur singulière dominante
    std::pair<T, torch::Tensor> powerMethod(
        const torch::Tensor& AtA, 
        int maxIter = 1000, 
        T tol = static_cast<T>(1e-7)) {
        
        int size = AtA.size(0);
        torch::Tensor v = torch::ones({size}, torch::TensorOptions().dtype(dtype).device(device));
        
        // Normalisation initiale
        v = v / torch::norm(v);
        
        T eigenvalue = static_cast<T>(0);
        
        for (int iter = 0; iter < maxIter; iter++) {
            torch::Tensor Av = torch::matmul(AtA, v);
            
            // Calcul de la valeur propre (v^T * A * v)
            T new_eigenvalue = torch::dot(v, Av).template item<T>();
            
            // Normalisation
            T Av_norm = torch::norm(Av).template item<T>();
            if (Av_norm < tol) break;
            
            Av = Av / Av_norm;
            
            // Test de convergence
            if (std::abs(new_eigenvalue - eigenvalue) < tol) {
                eigenvalue = new_eigenvalue;
                v = Av;
                break;
            }
            
            eigenvalue = new_eigenvalue;
            v = Av;
        }
        
        return {std::sqrt(std::max(static_cast<T>(0), eigenvalue)), v};
    }
    
    // Déflation de la matrice
    void deflate(torch::Tensor& A, 
                 const torch::Tensor& u, 
                 const torch::Tensor& v, 
                 T sigma) {
        
        // A = A - sigma * u * v^T
        torch::Tensor outer = torch::outer(u, v);
        A = A - sigma * outer;
    }

public:
    TensorSVD(int rows, int cols, torch::Device dev = torch::kCPU) 
        : m(rows), n(cols), device(dev), dtype(getScalarType()) {
        
        auto opts = torch::TensorOptions().dtype(dtype).device(device);
        U = torch::eye(m, opts);
        V = torch::eye(n, opts);
        S = torch::zeros({std::min(m, n)}, opts);
    }
    
    // Fonction principale pour calculer la SVD
    // ATTENTION: Cette méthode utilise des .item() qui causent des transferts GPU→CPU
    // Utilisez computeOptimized() pour de meilleures performances
    void compute(const torch::Tensor& A) {
        // Vérification des dimensions
        if (A.size(0) != m || A.size(1) != n) {
            throw std::invalid_argument("Dimensions de la matrice ne correspondent pas");
        }
        
        // Copie de la matrice pour les modifications
        torch::Tensor A_copy = A.clone().to(dtype).to(device);
        
        // Calcul de A^T * A
        torch::Tensor At = A_copy.transpose(0, 1);
        torch::Tensor AtA = torch::matmul(At, A_copy);
        
        // Réinitialisation des matrices U et V
        auto opts = torch::TensorOptions().dtype(dtype).device(device);
        U = torch::zeros({m, std::min(m, n)}, opts);
        V = torch::zeros({n, std::min(m, n)}, opts);
        S = torch::zeros({std::min(m, n)}, opts);
        
        int rank = std::min(m, n);
        
        // Calcul itératif des valeurs singulières
        for (int k = 0; k < rank; k++) {
            // Trouver la valeur singulière dominante et le vecteur propre correspondant
            auto [sigma, v_vec] = powerMethod(AtA);
            
            if (sigma < static_cast<T>(1e-7)) break; // Arrêter si la valeur singulière est trop petite
            
            S[k] = sigma;
            
            // Calculer u = A * v / sigma
            torch::Tensor u_vec = torch::matmul(A_copy, v_vec);
            T u_norm = torch::norm(u_vec).template item<T>();
            
            if (u_norm > static_cast<T>(1e-7)) {
                u_vec = u_vec / u_norm;
            }
            
            // Mettre à jour les matrices U et V
            U.index_put_({torch::indexing::Slice(), k}, u_vec);
            V.index_put_({torch::indexing::Slice(), k}, v_vec);
            
            // Déflation : A = A - sigma * u * v^T
            deflate(A_copy, u_vec, v_vec, sigma);
            
            // Recalculer A^T * A pour la prochaine itération
            At = A_copy.transpose(0, 1);
            AtA = torch::matmul(At, A_copy);
        }
        
        // Tri des valeurs singulières par ordre décroissant
        auto [S_sorted, indices] = torch::sort(S, 0, true); // true pour ordre décroissant
        
        // Réorganiser U et V selon l'ordre trié
        U = torch::index_select(U, 1, indices);
        V = torch::index_select(V, 1, indices);
        S = S_sorted;
    }
    
    // Version alternative utilisant directement les fonctions torch optimisées
    void computeOptimized(const torch::Tensor& A, int target_rank = -1) {
        torch::Tensor A_work = A.clone().to(dtype).to(device);

        if (device.type() == torch::kMPS) {
            // MPS: ni linalg_eigh ni linalg_qr ne sont supportés
            // Stratégie adaptative selon la taille et le rank demandé
            bool is_tall = (m > 2 * n) || (n > 2 * m);  // Matrice très rectangulaire
            bool low_rank = (target_rank > 0 && target_rank < std::min(m, n) / 3);

            if (low_rank && !is_tall) {
                // Pour rank partiel sur matrices carrées/peu rectangulaires:
                // Méthode eigendecomposition (A^T·A sur MPS + eigh sur CPU/MPS)
                computeMPSOptimized(A_work);
            } else {
                // Pour matrices très rectangulaires ou rank complet:
                // SVD directe sur CPU
                computeCPUFallback(A_work, target_rank);
            }
        } else {
            // Version standard pour CUDA/CPU avec linalg_eigh
            computeStandardOptimized(A_work);
        }
    }
    
private:
    // Fallback CPU complet - faire toute la SVD sur CPU avec Accelerate
    void computeCPUFallback(const torch::Tensor& A_work, int target_rank = -1) {
        torch::Tensor A_cpu = A_work.cpu();

        // torch::linalg_svd est optimisé avec Accelerate sur macOS
        auto svd_result = torch::linalg_svd(A_cpu, /*full_matrices=*/false);

        torch::Tensor U_full = std::get<0>(svd_result);
        torch::Tensor S_full = std::get<1>(svd_result);
        torch::Tensor Vt = std::get<2>(svd_result);

        // Si rank partiel demandé, tronquer
        if (target_rank > 0 && target_rank < static_cast<int>(S_full.size(0))) {
            int actual_rank = target_rank;
            U = U_full.slice(1, 0, actual_rank).to(device);
            S = S_full.slice(0, 0, actual_rank).to(device);
            V = Vt.transpose(0, 1).slice(1, 0, actual_rank).to(device);
        } else {
            U = U_full.to(device);
            S = S_full.to(device);
            V = Vt.transpose(0, 1).to(device);
        }
    }

    // Implémentation complète de linalg_eigh pour MPS using Divide-and-Conquer
    // OBSOLÈTE: Cette méthode contient des centaines de .item() qui tuent les performances
    // Elle n'est plus utilisée par computeOptimized()
    std::pair<torch::Tensor, torch::Tensor> mps_linalg_eigh(const torch::Tensor& A) {
        // A doit être une matrice symétrique
        int n = A.size(0);
        
        // Étape 1: Tridiagonalisation par réflexions de Householder
        auto [Tmat, Q] = householderTridiagonalization(A);
        
        // Étape 2: Divide-and-Conquer sur la matrice tridiagonale
        auto [eigenvals, Z] = divideConquerTridiagonal(Tmat);
        
        // Étape 3: Transformation finale des vecteurs propres
        torch::Tensor eigenvecs = torch::matmul(Q, Z);
        
        return std::make_pair(eigenvals, eigenvecs);
    }
    
private:
    // Tridiagonalisation de Householder complète
    std::pair<torch::Tensor, torch::Tensor> householderTridiagonalization(const torch::Tensor& A) {
        int n = A.size(0);
        torch::Tensor Tmat = A.clone();
        torch::Tensor Q = torch::eye(n, torch::TensorOptions().dtype(dtype).device(device));
        
        for (int k = 0; k < n - 2; k++) {
            // Extraire la colonne sous-diagonale
            torch::Tensor x = Tmat.slice(0, k + 1, n).slice(1, k, k + 1).squeeze(1);
            T alpha = torch::norm(x).template item<T>();
            
            if (alpha > static_cast<T>(1e-12)) {
                // Construire le vecteur de Householder
                torch::Tensor v = x.clone();
                T x0 = v[0].template item<T>();
                
                if (x0 >= static_cast<T>(0)) {
                    v[0] = x0 + alpha;
                } else {
                    v[0] = x0 - alpha;
                }
                
                T beta = torch::norm(v).template item<T>();
                if (beta > static_cast<T>(1e-12)) {
                    v = v / beta;
                    
                    // Appliquer la transformation de Householder
                    // H = I - 2*v*v^T
                    torch::Tensor vvt = torch::outer(v, v);
                    torch::Tensor H = torch::eye(n - k - 1, torch::TensorOptions().dtype(dtype).device(device)) - 2 * vvt;
                    
                    // Mettre à jour la sous-matrice
                    torch::Tensor sub_matrix = Tmat.slice(0, k + 1, n).slice(1, k + 1, n);
                    sub_matrix = torch::matmul(torch::matmul(H, sub_matrix), H);
                    Tmat.slice(0, k + 1, n).slice(1, k + 1, n) = sub_matrix;
                    
                    // Nettoyer les éléments sous-diagonaux
                    for (int i = k + 2; i < n; i++) {
                        Tmat[i][k] = static_cast<T>(0);
                        Tmat[k][i] = static_cast<T>(0);
                    }
                    
                    // Accumuler les transformations orthogonales
                    torch::Tensor Q_sub = Q.slice(1, k + 1, n);
                    Q.slice(1, k + 1, n) = torch::matmul(Q_sub, H);
                }
            }
        }
        
        return std::make_pair(Tmat, Q);
    }
    
    // Algorithme Divide-and-Conquer pour matrice tridiagonale
    std::pair<torch::Tensor, torch::Tensor> divideConquerTridiagonal(const torch::Tensor& Tmat) {
        int n = Tmat.size(0);
        
        // Cas de base
        if (n == 1) {
            torch::Tensor eigenval = torch::tensor({Tmat[0][0].template item<T>()}, 
                                                 torch::TensorOptions().dtype(dtype).device(device));
            torch::Tensor eigenvec = torch::ones({1, 1}, torch::TensorOptions().dtype(dtype).device(device));
            return std::make_pair(eigenval, eigenvec);
        }
        
        if (n == 2) {
            return solve2x2Tridiagonal(Tmat);
        }
        
        // Diviser au milieu
        int m = n / 2;
        
        // Extraire les sous-matrices
        torch::Tensor T1 = Tmat.slice(0, 0, m).slice(1, 0, m);
        torch::Tensor T2 = Tmat.slice(0, m, n).slice(1, m, n);
        
        // Élément de couplage
        T beta = Tmat[m][m-1].template item<T>();
        
        // Modifier les matrices pour découpler
        T1[m-1][m-1] = T1[m-1][m-1] - beta;
        T2[0][0] = T2[0][0] - beta;
        
        // Résoudre récursivement
        auto [d1, Q1] = divideConquerTridiagonal(T1);
        auto [d2, Q2] = divideConquerTridiagonal(T2);
        
        // Combiner les solutions
        return combineSubproblems(d1, Q1, d2, Q2, beta, m);
    }
    
    // Résolution directe pour matrice 2x2
    std::pair<torch::Tensor, torch::Tensor> solve2x2Tridiagonal(const torch::Tensor& Tmat) {
        T a = Tmat[0][0].template item<T>();
        T b = Tmat[0][1].template item<T>();
        T c = Tmat[1][1].template item<T>();
        
        // Calcul analytique des valeurs propres
        T trace = a + c;
        T det = a * c - b * b;
        T discriminant = trace * trace - 4 * det;
        
        T lambda1, lambda2;
        if (discriminant >= static_cast<T>(0)) {
            T sqrt_disc = std::sqrt(discriminant);
            lambda1 = (trace + sqrt_disc) / static_cast<T>(2);
            lambda2 = (trace - sqrt_disc) / static_cast<T>(2);
        } else {
            lambda1 = lambda2 = trace / static_cast<T>(2);
        }
        
        // Vecteurs propres
        torch::Tensor eigenvals = torch::tensor({lambda1, lambda2}, 
                                              torch::TensorOptions().dtype(dtype).device(device));
        torch::Tensor eigenvecs = torch::zeros({2, 2}, torch::TensorOptions().dtype(dtype).device(device));
        
        // Calcul des vecteurs propres
        for (int i = 0; i < 2; i++) {
            T lambda = (i == 0) ? lambda1 : lambda2;
            T denom = (a - lambda) * (a - lambda) + b * b;
            
            if (denom > static_cast<T>(1e-12)) {
                T v1 = b;
                T v2 = lambda - a;
                T norm = std::sqrt(v1 * v1 + v2 * v2);
                
                eigenvecs[0][i] = v1 / norm;
                eigenvecs[1][i] = v2 / norm;
            } else {
                eigenvecs[i][i] = static_cast<T>(1);
            }
        }
        
        return std::make_pair(eigenvals, eigenvecs);
    }
    
    // Combinaison des sous-problèmes (cœur de l'algorithme D&C)
    std::pair<torch::Tensor, torch::Tensor> combineSubproblems(
        const torch::Tensor& d1, const torch::Tensor& Q1,
        const torch::Tensor& d2, const torch::Tensor& Q2,
        T beta, int m) {
        
        int n1 = d1.size(0);
        int n2 = d2.size(0);
        int n = n1 + n2;
        
        // Construire la matrice diagonale combinée
        torch::Tensor d = torch::zeros({n}, torch::TensorOptions().dtype(dtype).device(device));
        d.slice(0, 0, n1) = d1;
        d.slice(0, n1, n) = d2;
        
        // Vecteur de couplage
        torch::Tensor z = torch::zeros({n}, torch::TensorOptions().dtype(dtype).device(device));
        z[n1-1] = static_cast<T>(1);
        z[n1] = static_cast<T>(1);
        
        // Résoudre l'équation séculaire: 1 + beta² * sum(z[i]² / (d[i] - lambda)) = 0
        torch::Tensor eigenvals = solveSecularEquation(d, z, beta);
        
        // Calculer les vecteurs propres mis à jour
        torch::Tensor eigenvecs = computeUpdatedEigenvectors(d, z, eigenvals, beta);
        
        // Transformer les vecteurs propres
        torch::Tensor Q_combined = torch::zeros({n, n}, torch::TensorOptions().dtype(dtype).device(device));
        Q_combined.slice(0, 0, n1).slice(1, 0, n1) = Q1;
        Q_combined.slice(0, n1, n).slice(1, n1, n) = Q2;
        
        eigenvecs = torch::matmul(Q_combined, eigenvecs);
        
        return std::make_pair(eigenvals, eigenvecs);
    }
    
    // Résolution de l'équation séculaire
    torch::Tensor solveSecularEquation(const torch::Tensor& d, const torch::Tensor& z, T beta) {
        int n = d.size(0);
        torch::Tensor eigenvals = torch::zeros({n}, torch::TensorOptions().dtype(dtype).device(device));
        
        // Tri des valeurs diagonales
        auto [d_sorted, indices] = torch::sort(d, 0);
        
        // Résolution par bissection pour chaque intervalle
        for (int k = 0; k < n; k++) {
            T left, right;
            
            if (k == 0) {
                left = d_sorted[0].template item<T>() - std::abs(beta);
            } else {
                left = d_sorted[k-1].template item<T>();
            }
            
            if (k == n - 1) {
                right = d_sorted[k].template item<T>() + std::abs(beta);
            } else {
                right = d_sorted[k+1].template item<T>();
            }
            
            // Bissection pour trouver la racine
            for (int iter = 0; iter < 50; iter++) {
                T mid = (left + right) / static_cast<T>(2);
                T f_mid = secularFunction(d, z, beta, mid);
                
                if (std::abs(f_mid) < static_cast<T>(1e-12)) break;
                
                if (f_mid > static_cast<T>(0)) {
                    right = mid;
                } else {
                    left = mid;
                }
            }
            
            eigenvals[k] = (left + right) / static_cast<T>(2);
        }
        
        return eigenvals;
    }
    
    // Fonction séculaire: f(lambda) = 1 + beta² * sum(z[i]² / (d[i] - lambda))
    T secularFunction(const torch::Tensor& d, const torch::Tensor& z, T beta, T lambda) {
        T sum = static_cast<T>(0);
        int n = d.size(0);
        
        for (int i = 0; i < n; i++) {
            T di = d[i].template item<T>();
            T zi = z[i].template item<T>();
            T denom = di - lambda;
            
            if (std::abs(denom) > static_cast<T>(1e-12)) {
                sum += (zi * zi) / denom;
            }
        }
        
        return static_cast<T>(1) + beta * beta * sum;
    }
    
    // Calcul des vecteurs propres mis à jour
    torch::Tensor computeUpdatedEigenvectors(const torch::Tensor& d, const torch::Tensor& z, 
                                           const torch::Tensor& eigenvals, T beta) {
        int n = d.size(0);
        torch::Tensor eigenvecs = torch::zeros({n, n}, torch::TensorOptions().dtype(dtype).device(device));
        
        for (int j = 0; j < n; j++) {
            T lambda = eigenvals[j].template item<T>();
            
            for (int i = 0; i < n; i++) {
                T di = d[i].template item<T>();
                T zi = z[i].template item<T>();
                T denom = di - lambda;
                
                if (std::abs(denom) > static_cast<T>(1e-12)) {
                    eigenvecs[i][j] = beta * zi / denom;
                }
            }
            
            // Normalisation
            T norm = torch::norm(eigenvecs.slice(1, j, j+1)).template item<T>();
            if (norm > static_cast<T>(1e-12)) {
                eigenvecs.slice(1, j, j+1) = eigenvecs.slice(1, j, j+1) / norm;
            }
        }
        
        return eigenvecs;
    }
    
    // Méthode optimisée spécifique pour MPS
    void computeMPSOptimized(const torch::Tensor& A_work) {
        // Calcul de A^T * A pour obtenir les vecteurs propres de droite (V) - sur MPS
        torch::Tensor AtA = torch::matmul(A_work.transpose(0, 1), A_work);

        // linalg_eigh non supporté sur MPS → CPU avec Accelerate (optimisé)
        torch::Tensor AtA_cpu = AtA.cpu();
        auto result = torch::linalg_eigh(AtA_cpu);
        torch::Tensor eigenvals_V = std::get<0>(result).to(device);
        torch::Tensor eigenvecs_V = std::get<1>(result).to(device);

        // Les valeurs singulières sont les racines carrées des valeurs propres
        torch::Tensor S_unsorted = torch::sqrt(torch::clamp(eigenvals_V, static_cast<T>(1e-7)));

        // IMPORTANT: Tri par ordre décroissant AVANT de calculer U
        auto [S_sorted, indices] = torch::sort(S_unsorted, 0, true);
        S = S_sorted;

        // Réorganisation des vecteurs propres selon le tri
        V = torch::index_select(eigenvecs_V, 1, indices);

        // VECTORISÉ: Calcul de U = A @ V (APRÈS le tri!) - sur MPS
        torch::Tensor U_unnorm = torch::matmul(A_work, V);

        // VECTORISÉ: Division par S (même formule que computeStandardOptimized)
        torch::Tensor S_safe = S.clamp_min(static_cast<T>(1e-7));
        U = U_unnorm / S_safe.unsqueeze(0);

        // Mettre à zéro les colonnes où S est trop petit
        torch::Tensor mask = S > static_cast<T>(1e-7);
        U = U * mask.unsqueeze(0);
    }
    
    // Méthode standard pour CUDA/CPU
    void computeStandardOptimized(const torch::Tensor& A_work) {
        // Calcul de A^T * A pour obtenir V
        torch::Tensor AtA = torch::matmul(A_work.transpose(0, 1), A_work);

        // Décomposition en valeurs propres de A^T * A pour V
        auto [eigenvals_V, eigenvecs_V] = torch::linalg_eigh(AtA);

        // Les valeurs singulières sont les racines carrées des valeurs propres
        S = torch::sqrt(torch::clamp(eigenvals_V, static_cast<T>(1e-7)));

        // Tri par ordre décroissant
        auto [S_sorted, indices] = torch::sort(S, 0, true);
        S = S_sorted;

        // Réorganisation des vecteurs propres
        V = torch::index_select(eigenvecs_V, 1, indices);

        // VECTORISÉ: Calcul de U = A @ V / S (toutes les colonnes en une fois)
        int rank = std::min(m, n);
        torch::Tensor U_unnorm = torch::matmul(A_work, V);

        // VECTORISÉ: Division par S (évite les .item() et la boucle)
        torch::Tensor S_safe = S.clamp_min(static_cast<T>(1e-7));
        U = U_unnorm / S_safe.unsqueeze(0);

        // Mettre à zéro les colonnes où S est trop petit
        torch::Tensor mask = S > static_cast<T>(1e-7);
        U = U * mask.unsqueeze(0);
    }
    
public:
    
    // Accesseurs pour récupérer les résultats
    const torch::Tensor& getU() const { return U; }
    const torch::Tensor& getV() const { return V; }
    const torch::Tensor& getS() const { return S; }
    
    // Fonction pour vérifier la reconstruction A = U * S * V^T
    torch::Tensor reconstruct() const {
        torch::Tensor S_diag = torch::diag(S);
        return torch::matmul(torch::matmul(U, S_diag), V.transpose(0, 1));
    }
    
    // Fonction pour calculer l'erreur de reconstruction
    T reconstructionError(const torch::Tensor& original) const {
        torch::Tensor reconstructed = reconstruct();
        torch::Tensor diff = original.to(dtype).to(device) - reconstructed;
        return torch::norm(diff).template item<T>();
    }
    
    // Fonction pour afficher les résultats
    void printResults() const {
        std::cout << "Valeurs singulières:" << std::endl;
        std::cout << S << std::endl;
        
        std::cout << "\nMatrice U:" << std::endl;
        std::cout << U << std::endl;
        
        std::cout << "\nMatrice V:" << std::endl;
        std::cout << V << std::endl;
    }
    
    // Fonction pour sauvegarder vers des tensors séparés (compatible avec le format torch)
    std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> tensors() const {
        return std::make_tuple(U.clone(), S.clone(), V.clone());
    }
};

// Fonctions utilitaires pour l'intégration avec lispe
namespace lispe_svd {
    
    // Fonction wrapper pour appel depuis lispe avec détection automatique du type
    inline std::tuple<torch::Tensor, torch::Tensor, torch::Tensor>
    compute_svd(const torch::Tensor& matrix, bool use_optimized = true,
                torch::Device target_device = torch::Device(torch::kCPU),
                int target_rank = -1) {

        int m = matrix.size(0);
        int n = matrix.size(1);

        // Utiliser le device de la matrice si target_device n'est pas spécifié explicitement
        torch::Device svd_device = (target_device.type() == torch::kCPU &&
                                   target_device.index() == -1) ?
                                  matrix.device() : target_device;

        // Détection automatique du type selon le device et les capacités
        if (svd_device.type() == torch::kMPS) {
            // MPS: Utiliser randomized SVD si rank partiel demandé
            if (matrix.scalar_type() == torch::kBFloat16) {
                TensorSVD<c10::BFloat16> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix, target_rank);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            } else {
                TensorSVD<float> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix, target_rank);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            }
        } else if (svd_device.type() == torch::kCUDA) {
            // CUDA supporte tous les types, choisir selon la matrice d'entrée ou float64 par défaut
            if (matrix.scalar_type() == torch::kBFloat16) {
                TensorSVD<c10::BFloat16> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            } else {
                TensorSVD<double> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            }
        } else {
            // CPU: utiliser double par défaut, ou bfloat16 si c'est le type d'entrée
            if (matrix.scalar_type() == torch::kBFloat16) {
                TensorSVD<c10::BFloat16> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            } else {
                TensorSVD<double> svd(m, n, svd_device);
                if (use_optimized) {
                    svd.computeOptimized(matrix);
                } else {
                    svd.compute(matrix);
                }
                return svd.tensors();
            }
        }
    }
    
    // Fonction pour calculer la SVD tronquée (k plus grandes valeurs singulières)
    inline std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> 
    compute_truncated_svd(const torch::Tensor& matrix, int k) {
        
        auto [U, S, V] = compute_svd(matrix, true);
        
        // Tronquer aux k premières composantes
        k = std::min(k, static_cast<int>(S.size(0)));
        
        torch::Tensor U_k = U.index({torch::indexing::Slice(), torch::indexing::Slice(0, k)});
        torch::Tensor S_k = S.index({torch::indexing::Slice(0, k)});
        torch::Tensor V_k = V.index({torch::indexing::Slice(), torch::indexing::Slice(0, k)});
        
        return std::make_tuple(U_k, S_k, V_k);
    }
    
    // Fonction pour calculer la pseudo-inverse en utilisant la SVD
    inline torch::Tensor pinverse_svd(const torch::Tensor& matrix, double threshold = 1e-10) {
        auto [U, S, V] = compute_svd(matrix, true);
        
        // Inverser les valeurs singulières non nulles
        torch::Tensor S_inv = torch::where(S > threshold, 1.0 / S, torch::zeros_like(S));
        
        // Pseudo-inverse = V * S^(-1) * U^T
        torch::Tensor S_inv_diag = torch::diag(S_inv);
        return torch::matmul(torch::matmul(V, S_inv_diag), U.transpose(0, 1));
    }
    
    // Fonction pour calculer une approximation de rang faible
    inline torch::Tensor low_rank_approximation(const torch::Tensor& matrix, int rank) {
        auto [U_k, S_k, V_k] = compute_truncated_svd(matrix, rank);
        
        // Reconstruction avec les k premières composantes
        torch::Tensor S_k_diag = torch::diag(S_k);
        return torch::matmul(torch::matmul(U_k, S_k_diag), V_k.transpose(0, 1));
    }
    
    // Fonction pour calculer la norme nucléaire (somme des valeurs singulières)
    inline double nuclear_norm(const torch::Tensor& matrix) {
        auto [U, S, V] = compute_svd(matrix, true);
        return torch::sum(S).item<double>();
    }
    
    // Fonction pour déterminer le rang effectif d'une matrice
    inline int effective_rank(const torch::Tensor& matrix, double threshold = 1e-7) {
        auto [U, S, V] = compute_svd(matrix, true);
        torch::Tensor mask = S > threshold;
        return torch::sum(mask).item<int>();
    }
}

// Exemple d'utilisation
inline void example_usage() {
    // Création d'une matrice d'exemple
    torch::Tensor A = torch::tensor({
        {1.0, 2.0},
        {3.0, 4.0},
        {5.0, 6.0}
    }, torch::kFloat64);
    
    std::cout << "Matrice originale:" << std::endl;
    std::cout << A << std::endl;
    
    // Calcul de la SVD
    auto [U, S, V] = lispe_svd::compute_svd(A);
    
    std::cout << "\nRésultats SVD:" << std::endl;
    std::cout << "U:\n" << U << std::endl;
    std::cout << "S:\n" << S << std::endl;
    std::cout << "V:\n" << V << std::endl;
    
    // Vérification de la reconstruction
    torch::Tensor reconstructed = torch::matmul(torch::matmul(U, torch::diag(S)), V.transpose(0, 1));
    std::cout << "\nMatrice reconstruite:" << std::endl;
    std::cout << reconstructed << std::endl;
    
    // Calcul de l'erreur
    double error = torch::norm(A - reconstructed).item<double>();
    std::cout << "\nErreur de reconstruction: " << error << std::endl;
    
    // Test de la pseudo-inverse
    torch::Tensor A_pinv = lispe_svd::pinverse_svd(A);
    std::cout << "\nPseudo-inverse:" << std::endl;
    std::cout << A_pinv << std::endl;
    
    // Test de l'approximation de rang faible
    torch::Tensor A_low_rank = lispe_svd::low_rank_approximation(A, 1);
    std::cout << "\nApproximation de rang 1:" << std::endl;
    std::cout << A_low_rank << std::endl;
    
    // Norme nucléaire et rang effectif
    double nuclear = lispe_svd::nuclear_norm(A);
    int rank = lispe_svd::effective_rank(A);
    std::cout << "\nNorme nucléaire: " << nuclear << std::endl;
    std::cout << "Rang effectif: " << rank << std::endl;
}

#endif // TENSOR_SVD_H