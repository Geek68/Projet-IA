# 🔍 Système Expert en Prolog – Enquête Policière

Ce projet est un petit système expert écrit en **Prolog** permettant de :
- Définir différents types de crimes (`vol`, `assassinat`, `escroquerie`).
- Associer des preuves à des suspects (mobile, empreintes, ADN, alibi...).
- Déterminer automatiquement si un suspect est **coupable** ou **non coupable**.
- Calculer un **niveau de suspicion** (score de 0 à 100).
- Garder un **historique des enquêtes**.

---

## 🚀 Fonctionnalités

- Vérification de culpabilité via la règle `is_guilty/2`.
- Calcul du **niveau de suspicion** avec `suspicion_level/3`.
- Interface simple en ligne de commande avec la prédicat `main/0`.
- Historique consultable avec `show_history/0`.

---

## 📂 Structure

- `crime_type/1` : Définit les crimes.
- `suspect/1` : Liste les suspects.
- `has_motive/2`, `has_dna_evidence/2`, etc. : Preuves liées aux suspects.
- `is_guilty/2` : Vérifie si un suspect est coupable.
- `suspicion_level/3` : Calcule le score de suspicion.
- `record_investigation/3` : Stocke l’historique.
- `show_history/0` : Affiche les enquêtes passées.

---

## 🛠️ Utilisation

1. **Lancer SWI-Prolog**
   ```bash
   swipl

2. **Charger le programme**
?- [enquetePoliciere].

3. **Taper le commande**
?- main.

4. **Commencer l'enquête**
Entrez une requête (ex: crime(john, vol)) ou "stop" pour quitter:
|: crime(john, vol).
john est coupable de vol.
Niveau de suspicion: 80%.

5. **Consulter l’historique des enquêtes**
?- show_history.


## 📊 Exemple de sortie
?- crime(mary, assassinat).
mary est coupable de assassinat.
Niveau de suspicion: 100%.

?- crime(sophie, escroquerie).
sophie n'est pas coupable de escroquerie.

?- show_history.
Historique des enquêtes:
john - vol: coupable
mary - assassinat: coupable
sophie - escroquerie: non_coupable