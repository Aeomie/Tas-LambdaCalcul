# Projet TAS - Évaluateur et Typeur d'un Lambda-Calcul Enrichi

## Informations Générales

**Auteur:** [Ounnoughi Kheireddine]  
**GitHub:** [https://github.com/Aeomie/Tas-LambdaCalcul](https://github.com/Aeomie/Tas-LambdaCalcul)

> **Note:** J'ai réalisé ce projet avec l'aide de chatbots (ChatGPT et Claude) pour déboguer et comprendre certaines parties complexes de l'implémentation.

---

## Parties Réalisées

### ✅ Partie 2 : Lambda-Calcul Simplement Typé
- Syntaxe inductive des termes (variables, abstractions, applications)
- Pretty printer pour les termes
- Alpha-conversion (convention de Barendregt)
- Substitution avec évitement de capture
- Évaluation Left-to-Right Call-by-Value
- Génération d'équations de typage
- Algorithme d'unification (avec occur check)
- Inférence de types

### ✅ Partie 3 : Extensions (Entiers, Listes, Let)
- **Entiers natifs** avec opérateurs `+` et `-`
- **Listes natives** avec `cons`, `hd`, `tl`
- **Branchements** `ifzero` et `ifempty`
- **Point fixe** `fix` pour la récursion
- **Let-binding** avec let-polymorphisme
- Généralisation des types (∀X.T)

### ✅ Partie 4 : Traits Impératifs
- **Références** avec `ref`, `!` (déréférencement), et `:=` (assignation)
- Sémantique à base d'états (régions mémoire)
- Type `Unit` et type `Ref T`
- **Polymorphisme faible** (non-expansivité)
- Gestion correcte des effets de bord

### ❌ Partie 5 : Extensions Supplémentaires
Non implémentée.

---

## Structure du Projet

```
.
├── ast.ml           # Définition des types (term, typ)
├── lexer.mll        # Analyseur lexical
├── parser.mly       # Analyseur syntaxique
├── main.ml          # Évaluateur, typeur et logique principale
├── Makefile         # Compilation automatique
├── exec.sh          # Script pour exécuter tous les tests
├── tests/           # Dossier contenant les fichiers de test
│   ├── fail_ones/       # Tests qui échouent
│   ├── Lists/       # Tests pour les lists
│   ├── section3/       # Tests pour la partie 3
│   └── ...
└── README.md        # Ce fichier
```

---

## Compilation

### Prérequis
- OCaml
- make

### Compiler le projet
```bash
make
```

Cela génère l'exécutable `main`.

### Nettoyer les fichiers générés
```bash
make clean
```

---

## Utilisation

### Syntaxe Générale
```bash
./main <fichier.lambda> <mode> <nombre_étapes>
```

### Paramètres
- `<fichier.lambda>` : Chemin vers le fichier source contenant le terme à évaluer/typer
- `<mode>` : 
  - `type` ou `ty` : Inférence de type uniquement
  - `eval` ou `ev` : Évaluation du terme
- `<nombre_étapes>` : Nombre maximal d'étapes de réduction (utilisé en mode `eval`)

### Exemples

#### Inférence de type
```bash
./main tests/add.lambda type 0
```

#### Évaluation
```bash
./main tests/add.lambda eval 50
```

---

## Script d'Exécution Automatique

Le script `exec.sh` permet d'exécuter tous les tests d'un dossier.

### Utilisation
```bash
./exec.sh <dossier> <mode> <nombre_étapes>
```

### Exemple
```bash
# Évaluer tous les tests de la partie 3
./exec.sh tests eval 30

# Typer tous les tests de la partie 4
./exec.sh tests type 0
```

### Fonctionnement du Script
Le script parcourt récursivement tous les dossiers et exécute chaque fichier `.lambda` avec les paramètres spécifiés. Il affiche :
- Le nom du fichier testé
- Le résultat de l'exécution
- Une séparation claire entre chaque test

---

## Syntaxe du Langage

### Termes de Base
```ocaml
(* Lambda-calcul *)
fun x -> x                      (* Abstraction *)
(fun x -> x) 42                 (* Application *)

(* Entiers et arithmétique *)
3 + 5                           (* Addition *)
10 - 3                          (* Soustraction *)

(* Listes *)
[]                              (* Liste vide *)
[1, 2, 3]                       (* Liste avec éléments *)
cons(1, [2, 3])                 (* Construction *)
hd([1, 2, 3])                   (* Tête *)
tl([1, 2, 3])                   (* Queue *)

(* Branchements *)
ifzero 0 then 10 else 20        (* Test de zéro *)
ifempty [] then 1 else 2        (* Test de liste vide *)

(* Let et récursion *)
let x = 5 in x + 3              (* Let-binding *)
fix (f -> fun n -> ...)         (* Point fixe *)

(* Références (impératif) *)
ref 42                          (* Création *)
!x                              (* Déréférencement *)
x := 100                        (* Assignation *)
()                              (* Unit *)
```



---

## Tests Fournis

### Organisation des Tests
Les tests sont organisés par partie dans le dossier `tests/` :

- **`tests/fail_ones/`** : Tests qui échouent
- **`tests/Lists/`** : Tests pour les lists
- **`tests/section3/`** : Tests pour la partie 3
- **`tests/section4/`** : Tests pour la partie 4

### Lancer Tous les Tests
```bash
# Typer tous les tests
./exec.sh tests type 0

# Évaluer tous les tests (50 étapes max)
./exec.sh tests eval 50
```

---


## Auteur et Remerciements

Ce projet a été réalisé dans le cadre du cours **TAS**.

Merci aux assistants IA (ChatGPT et Claude) qui m'ont aidé à déboguer et à mieux comprendre certaines parties de l'implémentation, notamment :
- L'algorithme d'unification
- La gestion du polymorphisme faible
- Les subtilités de la substitution avec évitement de capture
