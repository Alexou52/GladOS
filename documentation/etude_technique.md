# Étude Technique — GLaDOS

> **Projet** : GLaDOS (Generic Language and Data Operand Syntax)  
> **Type** : Interpréteur/Compilateur de langage fonctionnel  
> **Technologie principale** : Haskell  
> **Date** : Janvier 2026

---

## Table des matières

1. [Étude comparative des technologies](#1-étude-comparative-des-technologies)
2. [Veille et analyse de sécurité](#2-veille-et-analyse-de-sécurité)
3. [Prototypage et itérations](#3-prototypage-et-itérations)
4. [Architecture logicielle](#4-architecture-logicielle)
5. [Implémentation et bonnes pratiques](#5-implémentation-et-bonnes-pratiques)
6. [Organisation et structure du code](#6-organisation-et-structure-du-code)
7. [Algorithmes et solutions techniques](#7-algorithmes-et-solutions-techniques)
8. [Gestion de la persistance](#8-gestion-de-la-persistance)
9. [Choix des structures de données](#9-choix-des-structures-de-données)

---

## 1. Étude comparative des technologies

### 1.1 Benchmark des langages d'implémentation

Pour implémenter un interpréteur/compilateur de langage fonctionnel, nous avons mené une étude comparative approfondie de plusieurs options techniques :

| Langage | Paradigme | Typage | Performance | Parsing | Écosystème | Accessibilité | Note |
|---------|-----------|--------|-------------|---------|------------|---------------|------|
| **Haskell** ✓ | Fonctionnel pur | Statique fort | Excellente | Parsec natif | Mature | CLI accessible | **9/10** |
| OCaml | Fonctionnel | Statique fort | Très bonne | Menhir | Mature | CLI accessible | 8/10 |
| Rust | Multi-paradigme | Statique fort | Excellente | Nom/Pest | Croissance | CLI accessible | 8/10 |
| Python | Multi-paradigme | Dynamique | Moyenne | PLY/ANTLR | Très riche | Excellent | 6/10 |
| C/C++ | Impératif | Statique | Excellente | Flex/Bison | Très mature | Variable | 7/10 |

### 1.2 Justification du choix de Haskell

Notre choix s'est porté sur Haskell pour les raisons suivantes :

| Critère | Justification |
|---------|---------------|
| **Paradigme fonctionnel pur** | Correspondance naturelle avec la sémantique LISP ; transformations AST élégantes |
| **Types algébriques (ADT)** | Modélisation précise des AST, valeurs, et instructions bytecode |
| **Pattern matching exhaustif** | Le compilateur garantit le traitement de tous les cas |
| **Immutabilité par défaut** | Raisonnement simplifié, moins de bugs d'état |
| **Écosystème mature** | Stack, HSpec pour tests, bibliothèques de parsing |

**Inconvénients acceptés :**
- Courbe d'apprentissage initiale
- Debugging parfois moins intuitif
- Performances à surveiller (structures persistantes)

### 1.3 Prise en compte de l'accessibilité

L'étude comparative intègre les technologies compatibles avec les personnes en situation de handicap :

| Besoin d'accessibilité | Technologie choisie | Justification |
|------------------------|---------------------|---------------|
| **Lecture d'écran** | CLI textuelle + Markdown | Sorties textuelles structurées, pas de dépendance visuelle |
| **Navigation clavier** | Site web HTML sémantique | Navigation Tab, pas de JavaScript obligatoire pour lire |
| **Troubles cognitifs** | Messages d'erreur explicites | Contexte fichier/ligne, hints, pas d'ambiguïté |
| **Daltonisme** | Codes ANSI optionnels | Le backend playground supprime les couleurs ANSI |
| **Motricité réduite** | Interface minimaliste | Pas de gestes complexes requis |

**Implémentation concrète :**

```
┌─────────────────────────────────────────────────────────────┐
│                    Accessibilité GLaDOS                     │
├─────────────────────────────────────────────────────────────┤
│  CLI                                                        │
│  ├─ Messages d'erreur : fichier:ligne:colonne + contexte    │
│  ├─ Sortie textuelle pure (compatible lecteurs d'écran)     │
│  └─ Pas de dépendance à la couleur pour comprendre          │
├─────────────────────────────────────────────────────────────┤
│  Documentation                                              │
│  ├─ Format Markdown → HTML sémantique                       │
│  ├─ Structure hiérarchique (h1, h2, h3...)                  │
│  └─ Liens explicites, pas d'icônes seules                   │
├─────────────────────────────────────────────────────────────┤
│  Playground Web                                             │
│  ├─ Formulaire standard (label + textarea)                  │
│  ├─ Résultats en texte brut                                 │
│  └─ Navigation clavier complète                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Veille et analyse de sécurité

### 2.1 Étude des failles de sécurité récentes (2024-2026)

Notre veille sécurité couvre les vulnérabilités pertinentes pour les technologies utilisées dans le projet :

#### Vulnérabilités analysées

| CVE / Advisory | Technologie | Type | Sévérité | Impact sur GLaDOS |
|----------------|-------------|------|----------|-------------------|
| **CVE-2024-3094** | XZ Utils | Backdoor supply chain | Critique | Audit dépendances Haskell |
| **GHSA-rrrm-qjm4-v8hf** | marked.js | ReDoS | Moyenne | Mise à jour CDN recommandée |
| **GHSA-m2qf-hxjv-5gpq** | Flask | Session fixation | Élevée | Backend playground à surveiller |
| **CVE-2024-21626** | Container runtimes | Escape | Élevée | Sandboxing VM renforcé |
| **CVE-2024-4577** | PHP-CGI | RCE | Critique | Non applicable (pas de PHP) |

#### Actualités sécurité informatique pertinentes

| Événement | Date | Leçon pour le projet |
|-----------|------|---------------------|
| Attaque supply chain XZ Utils | Mars 2024 | Minimiser les dépendances, auditer |
| Vulnérabilités GHC corrigées | 2024-2025 | Maintenir le compilateur à jour |
| Recommandations ANSSI | 2024 | Haskell reconnu pour la sûreté |
| Failles Markdown parsers | 2024-2025 | Sanitizer ou désactiver HTML brut |

### 2.2 Modèle de menace

```
┌─────────────────────────────────────────────────────────────┐
│                    Surface d'attaque                        │
├──────────────────────────┬──────────────────────────────────┤
│   Binaire CLI (local)    │   Playground Web (distant)       │
├──────────────────────────┼──────────────────────────────────┤
│ • Fichier .gla/.scm      │ • Code utilisateur via HTTP      │
│   malveillant            │ • Requêtes POST malformées       │
│ • Boucle infinie         │ • DoS (timeout bypass)           │
│ • Stack overflow         │ • XSS via sortie d'erreur        │
│ • Input pathologique     │ • Supply chain (CDN, pip)        │
└──────────────────────────┴──────────────────────────────────┘
```

### 2.3 Mesures de sécurité implémentées

| Risque | Mitigation | Fichier |
|--------|------------|---------|
| **Boucle infinie** | Timeout 5 secondes | `website/backend.py` |
| **Explosion de sortie** | Limite taille output | `website/backend.py` |
| **Injection XSS** | Nettoyage ANSI, échappement | `website/backend.py` |
| **Stack overflow** | Limites de profondeur implicites | `src/VM.hs` |
| **Accès système** | Aucun builtin d'I/O fichier | `src/VM.hs` |

**Exemple de code de protection :**

```python
MAX_EXECUTION_TIME = 5      # Timeout en secondes
MAX_OUTPUT_SIZE = 50000     # Limite de sortie

# Nettoyage des codes ANSI pour accessibilité et sécurité
def strip_ansi(text):
    ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)
```

### 2.4 Recommandations de durcissement

Pour un déploiement production du playground :

1. **Sandboxing** : Conteneur Docker avec seccomp/AppArmor
2. **CSP** : Content-Security-Policy stricte
3. **Rate limiting** : Limiter les requêtes par IP
4. **Audit dépendances** : `pip-audit`, `stack audit`
5. **HTTPS** : Certificat TLS obligatoire

---

## 3. Prototypage et itérations

Le développement de GLaDOS a suivi une approche itérative avec trois prototypes successifs, chacun apportant des fonctionnalités et des apprentissages.

### 3.1 Prototype 1 — Interpréteur LISP simple

**Objectif :** Valider la faisabilité avec S-expressions

**Fichiers concernés :** `src/Reader.hs`, `src/Parser.hs`, `src/Eval.hs`

| Avantages | Inconvénients |
|-----------|---------------|
| ✅ Implémentation rapide (2 semaines) | ❌ Performances limitées |
| ✅ Code simple et lisible | ❌ Pas de boucles (`for`, `while`) |
| ✅ Sémantique LISP validée | ❌ Messages d'erreur basiques |
| ✅ Tests faciles à écrire | ❌ Pas de persistance bytecode |

**Conclusion :** Bon pour valider le concept, insuffisant pour les fonctionnalités avancées.

### 3.2 Prototype 2 — Syntaxe Python-like + Bytecode

**Objectif :** Ajouter une syntaxe moderne et compiler vers bytecode

**Fichiers concernés :** `src/Lexer.hs`, `src/NewParser.hs`, `src/Compiler.hs`, `src/VM.hs`

| Avantages | Inconvénients |
|-----------|---------------|
| ✅ Syntaxe accessible (Python-like) | ❌ Complexité accrue |
| ✅ Boucles `for`, `while`, `break`, `continue` | ❌ Debugging bytecode plus difficile |
| ✅ VM à pile performante | ❌ Temps de développement doublé |
| ✅ Séparation compilation/exécution | ❌ Deux parsers à maintenir |

**Conclusion :** Répond aux exigences fonctionnelles, architecture maintenable.

### 3.3 Prototype 3 — Persistance bytecode (.glc)

**Objectif :** Compiler une fois, exécuter plusieurs fois

**Fichier concerné :** `src/BytecodeFile.hs`

| Avantages | Inconvénients |
|-----------|---------------|
| ✅ Accélère le cycle d'exécution | ❌ Format binaire à maintenir |
| ✅ Distribution sans sources | ❌ Obfuscation XOR ≠ sécurité réelle |
| ✅ Checksum + version | ❌ Compatibilité ascendante à gérer |

**Conclusion :** Utile pour l'usage intensif, versioning bien géré.

### 3.4 Synthèse des prototypes

```
Prototype 1          Prototype 2          Prototype 3
    │                    │                    │
    ▼                    ▼                    ▼
┌─────────┐        ┌───────────┐        ┌───────────┐
│ Lisp    │   →    │ + Bytecode│   →    │ + .glc    │
│ simple  │        │ + VM      │        │ persist   │
└─────────┘        └───────────┘        └───────────┘
    │                    │                    │
    └────────────────────┴────────────────────┘
                         │
                         ▼
              ┌─────────────────────┐
              │   GLaDOS Final      │
              │ (2 syntaxes, VM,    │
              │  bytecode, .glc)    │
              └─────────────────────┘
```

---

## 4. Architecture logicielle

### 4.1 Architecture retenue : Monolithe modulaire en pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                         GLaDOS - Architecture                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  Entrée .scm (LISP)              Entrée .gla (Python-like)          │
│        │                                │                            │
│        ▼                                ▼                            │
│  ┌──────────┐                    ┌──────────┐                       │
│  │ Reader   │                    │ Lexer    │                       │
│  │ Parser   │                    │ NewParser│                       │
│  └────┬─────┘                    └────┬─────┘                       │
│       │                               │                              │
│       └───────────┬───────────────────┘                              │
│                   ▼                                                  │
│            ┌──────────┐                                             │
│            │   AST    │  (représentation unifiée)                   │
│            └────┬─────┘                                             │
│                 │                                                    │
│       ┌─────────┴─────────┐                                         │
│       ▼                   ▼                                         │
│  ┌──────────┐       ┌──────────┐                                   │
│  │  Eval    │       │ Compiler │                                   │
│  │ (interp) │       └────┬─────┘                                   │
│  └────┬─────┘            │                                         │
│       │                  ▼                                          │
│       │            ┌──────────┐      ┌──────────────┐              │
│       │            │ Bytecode │ ←──→ │ BytecodeFile │              │
│       │            └────┬─────┘      │   (.glc)     │              │
│       │                 │            └──────────────┘              │
│       │                 ▼                                          │
│       │            ┌──────────┐                                    │
│       │            │    VM    │                                    │
│       │            └────┬─────┘                                    │
│       │                 │                                          │
│       └────────┬────────┘                                          │
│                ▼                                                    │
│           ┌──────────┐                                             │
│           │ Résultat │                                             │
│           └──────────┘                                             │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 4.2 Justification de l'architecture

**Pourquoi un monolithe et pas des micro-services ?**

| Critère | Monolithe | Micro-services |
|---------|-----------|----------------|
| Complexité opérationnelle | ✅ Faible | ❌ Élevée |
| Latence | ✅ Nulle (in-process) | ❌ Réseau |
| Déploiement | ✅ Un binaire | ❌ Orchestration |
| Cohérence données | ✅ Garantie | ❌ Eventual consistency |
| Pertinence projet | ✅ Interpréteur = 1 unité logique | ❌ Pas de domaines indépendants |

Le projet n'a pas de domaines métier distincts nécessitant une séparation réseau. Le monolithe modulaire offre la simplicité d'un binaire unique tout en conservant une architecture interne claire.

### 4.3 Intégration dans l'écosystème technique

```
┌─────────────────────────────────────────────────────────────┐
│                    Écosystème GLaDOS                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────┐     ┌─────────────┐     ┌─────────────┐   │
│   │   Stack     │────▶│   GHC       │────▶│   glados    │   │
│   │  (build)    │     │ (compiler)  │     │  (binary)   │   │
│   └─────────────┘     └─────────────┘     └─────────────┘   │
│         │                                       │           │
│         ▼                                       ▼           │
│   ┌─────────────┐                       ┌─────────────┐     │
│   │   HSpec     │                       │   Flask     │     │
│   │  (tests)    │                       │ (backend)   │     │
│   └─────────────┘                       └─────────────┘     │
│                                               │             │
│                                               ▼             │
│                                         ┌─────────────┐     │
│                                         │ Playground  │     │
│                                         │   (HTML)    │     │
│                                         └─────────────┘     │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 5. Implémentation et bonnes pratiques

### 5.1 Respect de l'architecture

L'implémentation suit exactement l'architecture présentée :

| Module | Responsabilité | Fichier |
|--------|----------------|---------|
| Lexer | Tokenisation `.gla` | `src/Lexer.hs` |
| NewParser | AST depuis tokens | `src/NewParser.hs` |
| Reader | Lecture `.scm` | `src/Reader.hs` |
| Parser | AST depuis S-expr | `src/Parser.hs` |
| AST | Types de l'arbre | `src/AST.hs` |
| Compiler | AST → Bytecode | `src/Compiler.hs` |
| Bytecode | Instructions VM | `src/Bytecode.hs` |
| VM | Exécution bytecode | `src/VM.hs` |
| BytecodeFile | Sérialisation .glc | `src/BytecodeFile.hs` |
| Eval | Interprétation directe | `src/Eval.hs` |

### 5.2 Bonnes pratiques appliquées

#### Clean Code

```haskell
-- ✅ Noms explicites
compileExpr :: Ast -> Program
compileExpr (AstInt n) = [PUSH (VInt n)]
compileExpr (AstBool b) = [PUSH (VBool b)]
compileExpr (AstSymbol name) = [LOAD name]

-- ✅ Fonctions courtes et focalisées
-- ✅ Pattern matching exhaustif (le compilateur vérifie)
```

#### Design Patterns utilisés

| Pattern | Utilisation | Fichier |
|---------|-------------|---------|
| **Interpreter** | Évaluation de l'AST | `src/Eval.hs` |
| **Visitor** (via pattern matching) | Compilation AST→Bytecode | `src/Compiler.hs` |
| **State Machine** | VM à pile | `src/VM.hs` |
| **Factory** (combinateurs) | Construction de parsers | `src/NewParser.hs` |

#### Paradigme fonctionnel

```haskell
-- ✅ Fonctions pures
compileExpr :: Ast -> Program  -- Pas d'effets de bord

-- ✅ Gestion d'erreurs explicite avec Either
runVM :: VMState -> Either String Value

-- ✅ Composition de fonctions
compile = concatMap compileExpr
```

---

## 6. Organisation et structure du code

### 6.1 Structure rationnelle des fichiers

```
src/
├── Main.hs           # Point d'entrée, orchestration
├── AST.hs            # Types de l'arbre syntaxique
├── SExpr.hs          # S-expressions LISP
├── Reader.hs         # Lecture fichiers source
├── Parser.hs         # Parser S-expressions
├── Lexer.hs          # Lexer nouvelle syntaxe
├── NewParser.hs      # Parser nouvelle syntaxe
├── Eval.hs           # Interpréteur direct
├── Compiler.hs       # Compilateur bytecode
├── Bytecode.hs       # Instructions VM
├── BytecodeFile.hs   # Sérialisation .glc
├── VM.hs             # Machine virtuelle
└── ErrorMessage.hs   # Formatage erreurs

test/
├── Spec.hs           # Runner de tests
├── LexerSpec.hs      # Tests lexer
├── NewParserSpec.hs  # Tests parser
├── CompilerSpec.hs   # Tests compilateur
├── VMSpec.hs         # Tests VM
└── ...
```

### 6.2 Objectifs de la segmentation

| Objectif | Comment c'est atteint |
|----------|----------------------|
| **Performance** | VM à pile optimisée, bytecode compact |
| **Adaptabilité** | Ajout d'instructions sans toucher au parser |
| **Maintenabilité** | Modules < 300 lignes, responsabilité unique |

---

## 7. Algorithmes et solutions techniques

### 7.1 Algorithmes standards optimaux

| Problème | Algorithme | Complexité | Fichier |
|----------|------------|------------|---------|
| Exécution bytecode | Machine à pile | O(n) | `src/VM.hs` |
| Lookup variables | Map (arbre binaire) | O(log n) | `src/VM.hs` |
| Parsing | Descente récursive | O(n) | `src/NewParser.hs` |
| Tokenisation | Automate linéaire | O(n) | `src/Lexer.hs` |

### 7.2 Solutions originales

#### Résolution des sauts de boucle

Le compilateur génère d'abord des instructions `BREAK` et `CONTINUE` placeholder, puis les résout en `JMP` avec les bons offsets :

```haskell
-- Algorithme : résolution en 2 passes
resolveWhileLoopJumps :: [Instruction] -> Int -> [Instruction]
resolveWhileLoopJumps body loopBackOffset = map resolve body
  where
    bodyLen = length body
    resolve BREAK = JMP (bodyLen - idx + 1)  -- Saute après la boucle
    resolve CONTINUE = JMP loopBackOffset    -- Retourne au test
    resolve other = other
```

#### Compilation des boucles `for`

```haskell
-- for x in iterable:
--     body
-- 
-- Compilé en :
-- __iter = iterable
-- __idx = 0
-- loop_start:
--     if __idx >= len(__iter): goto loop_end
--     x = get(__iter, __idx)
--     body
--     __idx = __idx + 1
--     goto loop_start
-- loop_end:
```

---

## 8. Gestion de la persistance

### 8.1 Besoins de persistance

| Donnée | Besoin | Solution |
|--------|--------|----------|
| Code source | Lecture seule | Fichiers `.gla`, `.scm` |
| Bytecode compilé | Cache optionnel | Fichiers `.glc` |
| Variables runtime | Volatile | Mémoire (Map) |
| Données utilisateur | Aucun | Pas de base de données |

### 8.2 Justification : pas de base de données

Le projet est un **interpréteur/compilateur**, pas une application métier :

- ✅ Pas de données utilisateur à persister
- ✅ Pas de sessions ou d'authentification
- ✅ Chaque exécution est indépendante
- ✅ Complexité d'une BDD non justifiée

### 8.3 Format de persistance bytecode (.glc)

```
┌─────────────────────────────────────────┐
│           Format .glc                   │
├─────────────────────────────────────────┤
│ Magic Number (4 bytes)    "GLC\0"       │
│ Version (2 bytes)         0x0001        │
│ Checksum (4 bytes)        CRC32         │
│ Taille instructions (4 bytes)           │
│ Instructions (variable)   XOR encodé    │
│ Constantes (variable)                   │
└─────────────────────────────────────────┘
```

**Note :** L'obfuscation XOR n'est pas une protection cryptographique. Elle décourage simplement l'édition manuelle accidentelle.

---

## 9. Choix des structures de données

### 9.1 Structures choisies

| Structure | Type Haskell | Usage | Justification |
|-----------|--------------|-------|---------------|
| **Pile VM** | `[Value]` | Opérandes | Push/pop O(1) en tête de liste |
| **Environnement** | `Map String Value` | Variables | Lookup O(log n), immutable |
| **Programme** | `[Instruction]` | Bytecode | Accès séquentiel, indexé par PC |
| **AST** | ADT `Ast` | Arbre syntaxique | Pattern matching exhaustif |
| **Valeurs** | ADT `Value` | Données runtime | Type-safe, extensible |

### 9.2 Types algébriques (ADT)

```haskell
-- Valeurs manipulées par la VM
data Value
    = VInt Int
    | VBool Bool
    | VString String
    | VList [Value]
    | VClosure [String] Program
    | VBuiltin String
    | VNil

-- Instructions bytecode
data Instruction
    = PUSH Value | POP | DUP
    | LOAD String | STORE String
    | ADD | SUB | MUL | DIV | MOD | POW | FACT
    | EQ_ | LT_ | GT_ | LTE | GTE | NEQ
    | JMP Int | JMP_IF_FALSE Int | JMP_IF_TRUE Int
    | CALL Int | RET
    | MAKE_CLOSURE [String] Program
    | PRINT | HALT | LINE Int
    -- ...
```

### 9.3 Justification des choix

| Critère | Choix | Alternative rejetée | Raison |
|---------|-------|---------------------|--------|
| **Performance** | Liste pour pile | Array mutable | Immutabilité + GC suffisant |
| **Maintenabilité** | ADT pour valeurs | Tagged union C-style | Pattern matching sûr |
| **Évolutivité** | Map pour env | Liste d'association | O(log n) vs O(n) |

---

## Synthèse

Ce document présente l'étude technique complète du projet GLaDOS, couvrant :

- Une **étude comparative** rigoureuse des technologies avec prise en compte de l'accessibilité
- Une **veille sécurité** active et des mesures de protection implémentées
- Un **processus de prototypage** itératif avec arbitrages documentés
- Une **architecture** monolithe modulaire justifiée et intégrée à l'écosystème
- Une **implémentation** respectant les bonnes pratiques et design patterns
- Une **organisation du code** claire et maintenable
- Des **algorithmes** optimaux et des solutions originales
- Des **choix de persistance** cohérents avec les besoins
- Des **structures de données** justifiées et adaptées

---

*Document rédigé pour le projet GLaDOS — Janvier 2026*
