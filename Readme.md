# Rapport de Projet Micro Go : Compilateur vers MIPS

## Auteurs
- RIGGI Quentin
- VIAL Gustave

## Organisation du travail
Nous avons réalisé ce projet en binôme. Nous avons utilisé Git pour la gestion de versions et la coordination. Le développement s'est déroulé en suivant la chaîne de compilation logique :

>- D'abord l'analyse lexicale et syntaxique (Front-end).
>- Puis le vérificateur de types (Typage).
>- Enfin la génération de code MIPS (Back-end) et les tests avec SPIM.

**Résumé du travail effectué**
>- Gestion de l'insertion automatique de point-virgules dans le lexer.
>- Analyse syntaxique et construction de l'AST avec Menhir.
>- Typage statique avec support de la récursivité mutuelle (structures et fonctions).
>- Compilation des structures (allocation dynamique sur le tas).
>- Gestion des appels de fonctions et des retours multiples via la pile.
>- Création de fonctions d'affichage polymorphes pour fmt.Print.

<br><br>

## Approfondissement technique
#### Le défi du Point-Virgule Automatique
- La spécification de Go impose d'insérer un ; virtuel dans certains cas, notamment avant une accolade fermante }. Cela a posé problème car le lexer ne renvoie qu'un token à la fois.

```OCaml
let last_token = ref EOF
let next_token = ref None (* buffer pour le cas } sans \n *)
...
rule real_token = parse 
| "}" { 
    match !last_token with 
    | IDENT _ | INT _ | ... -> (* Si le token précédent finit une instruction *) 
        next_token := Some RBRACKET; (* On garde le } pour plus tard *) 
        last_token := SEMI; SEMI (* On renvoie le ; maintenant *) 
    | _ -> mem_token RBRACKET 
    }
```
**Regardons étape par étape :**

1. **Détection du contexte :** On regarde !last_token. Si c'était un identifiant ou un entier, l'instruction est finie.
2. **Injection du point-virgule :** On renvoie SEMI immédiatement pour satisfaire la grammaire.
3. **Mise en tampon :** Comme on a "mangé" l'accolade } pour renvoyer ; on la stocke dans next_token pour la renvoyer au prochain appel de la fonction.

<br>

#### Compilation des Structures et Allocation
- Pour compiler new(S), nous devons allouer de la mémoire sur le tas (Heap) et initialiser les champs à 0.

```Ocaml
| New s_name ->
      let layout = StringMap.find s_name !struct_table in
      (* Allocation dynamique via syscall 9 (sbrk) *)
      let alloc = li a0 layout.size @@ li v0 9 @@ syscall @@ move t0 v0 in
      
      (* Boucle d'initialisation à 0 *)
      let init_loop = 
        li t1 0 
        @@ label loop_lbl 
        @@ li t2 layout.size 
        @@ bge t1 t2 end_lbl
        ...
```


**La logique d'allocation :**

1. **Calcul de la taille :** Lors de la phase de déclaration, nous avons calculé la taille totale de chaque structure (somme des tailles des champs).
2. **Syscall 9 :** On utilise l'appel système MIPS sbrk pour demander layout.size octets au système.
3. **Initialisation :** Une boucle parcourt cette zone mémoire pour tout mettre à 0 (équivalent de nil ou 0), garantissant une initialisation propre.

<br>

#### Gestion des Retours Multiples sur la Pile
- Micro Go permet return a, b. MIPS ne possède que \$v0 (et \$v1) pour les retours standards. Nous avons dû innover.


```Ocaml
(* 1. Allocation place résultats *)
let alloc_res = 
  let rec loop i = if i = 0 then nop else (push zero) @@ loop (i-1) in 
  loop n_rets 
(* 2. Passage des pointeurs vers ces places *)
let push_ptrs = let rec loop i = ... addi t0 sp off @@ push t0 @@ loop (i + 1) in loop 0
```

**Regardons étape par étape :**

1. **Réservation :** L'appelant empile des zéros (push zero) pour réserver l'espace où seront stockés les résultats.

2. **Pointeurs :** Il calcule l'adresse de ces espaces réservés (addi t0 sp off) et empile ces adresses.

3. **Écriture :** La fonction appelée récupère ces adresses et utilise sw (store word) pour écrire les résultats directement dans la pile de l'appelant.

<br><br>

## Prise de recul
#### Difficultés rencontrées :
- La première difficulté majeure fut la gestion de la pile. Un simple décalage de 4 octets dans le calcul des offsets (\$fp vs \$sp) suffisait à corrompre les variables locales ou les adresses de retour. Nous avons passé beaucoup de temps à dessiner l'état de la pile sur papier pour déboguer les segfaults dans SPIM.

- La seconde difficulté concernait l'astuce du point-virgule automatique dans le lexer (décrite plus haut). Comprendre que le parser échouait sur } else parce qu'il manquait un ; virtuel a nécessité une analyse fine de la grammaire.

<br>

#### Pour aller plus loin :
Si nous avions disposé de plus de temps, nous aurions aimé ajouter :

>- **Optimisation de l'utilisation des registres :** Actuellement, notre compilateur utilise énormément la pile (push/pop). Utiliser des registres temporaires (\$t0-\$t9) pour les calculs intermédiaires réduirait considérablement le nombre d'instructions.

>- **Garbage Collector :** La mémoire allouée par new n'est jamais libérée. Un mécanisme simple de libération mémoire serait nécessaire pour des programmes plus longs.

>- **Contrôle de boucle avancé :** Ajouter le support des instructions break et continue, ce qui demanderait de propager les labels de fin de boucle dans l'environnement de compilation.