# 🔍 Système Expert d'Enquête Policière

Ce projet est un système expert qui utilise un moteur logique en **Prolog** et une interface web moderne et responsive pour simuler une enquête policière. L'utilisateur peut sélectionner un crime et un suspect, et le système déduit la culpabilité en se basant sur un ensemble de faits et de règles.

---

##  Architectural Overview

L'application est composée de deux parties principales :

1.  **Backend (Moteur Logique)** : Écrit en **SWI-Prolog**, le fichier `enquetePoliciere.pl` contient :
    *   Les **faits** : suspects, preuves (mobiles, empreintes, ADN, etc.), alibis.
    *   Les **règles de déduction** (`is_guilty/2`) pour déterminer la culpabilité en fonction des preuves.
    *   Un **serveur HTTP** qui expose une API sur le port `8000` pour que l'interface web puisse communiquer avec le moteur logique.

2.  **Frontend (Interface Utilisateur)** : Une interface web monopage (`index.html`) conçue pour être intuitive et mobile-friendly. Elle utilise :
    *   **HTML** pour la structure.
    *   **TailwindCSS** pour un design moderne et responsive.
    *   **JavaScript** (`script.js`) pour interroger le backend Prolog, afficher les résultats de l'enquête (verdict, preuves, niveau de suspicion) et maintenir un historique des enquêtes menées.

---

## 🚀 Comment Lancer l'Application

Pour faire fonctionner l'application, vous devez lancer le serveur Prolog, puis ouvrir l'interface web dans votre navigateur.

### Prérequis

- **SWI-Prolog** : Vous devez avoir installé SWI-Prolog sur votre machine. Vous pouvez le télécharger depuis le [site officiel de SWI-Prolog](https://www.swi-prolog.org/download/stable).

### Étape 1 : Démarrer le Serveur Backend

1.  Ouvrez un terminal ou une invite de commande.
2.  Naviguez jusqu'au répertoire du projet :
    ```bash
    cd chemin/vers/votre/Projet-IA
    ```
3.  Lancez le serveur Prolog en utilisant la commande suivante :
    ```
    swipl -s enquetePoliciere.pl -g main
    ```
    Cette commande charge le fichier `enquetePoliciere.pl` et exécute le prédicat `main`, qui démarre le serveur.

4.  Vous devriez voir un message confirmant que le serveur est lancé :
    ```
    Serveur lancé sur http://localhost:8000/
    ```
    **Laissez ce terminal ouvert.** Il fait tourner votre backend.

### Étape 2 : Utiliser l'Interface Web

1.  Une fois le serveur démarré, ouvrez votre navigateur web (Chrome, Firefox, etc.).
2.  Ouvrez le fichier `index.html` directement dans le navigateur. Vous pouvez soit double-cliquer sur le fichier, soit taper son chemin d'accès dans la barre d'adresse :
    ```
    file:///D:/Fianarana%20ambony/Projets/M1/Prolog/Projet-IA/index.html
    ```
    *(Adaptez le chemin si nécessaire)*

---

## 🛠️ Utilisation de l'Interface

L'interface est simple et intuitive :

1.  **Panneau d'investigation** :
    *   Utilisez le menu déroulant **"Type de crime"** pour choisir entre `Vol`, `Assassinat` ou `Escroquerie`.
    *   Utilisez le menu déroulant **"Suspect"** pour choisir qui enquêter.

2.  **Lancer l'enquête** :
    *   Cliquez sur le bouton **"Lancer l'enquête"**.

3.  **Résultats** :
    *   La section **"Résultats de l'enquête"** se mettra à jour et affichera :
        *   Le **verdict** (Coupable ou Non coupable).
        *   Le **niveau de suspicion** sous forme de pourcentage et de barre de progression.
        *   La liste des **preuves** trouvées qui ont mené à cette conclusion.

4.  **Historique** :
    *   Chaque enquête est automatiquement ajoutée au tableau **"Historique des enquêtes"** en bas de la page, vous permettant de conserver une trace de vos investigations.
