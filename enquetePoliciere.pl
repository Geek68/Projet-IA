% Types de crime
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

% Directive pour éviter les avertissements (optionnel)
:- discontiguous has_motive/2.
:- discontiguous was_near_crime_scene/2.
:- discontiguous has_fingerprint_on_weapon/2.
:- discontiguous eyewitness_identification/2.

% Faits
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).
eyewitness_identification(mary, assassinat).

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).

has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% Règles
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    has_fingerprint_on_weapon(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    ( has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ).

% Entrée principale
main :-
    repeat,  % Boucle infinie
    write('Entrez une requête (ex: crime(john, vol).) ou "stop." pour quitter:'), nl,
    read(Input),
    (Input = stop -> true ;  % Condition de sortie
    (crime(Suspect, CrimeType) = Input ->
        (is_guilty(Suspect, CrimeType) ->
            format('~w est coupable de ~w.~n', [Suspect, CrimeType])
        ;   format('~w n\'est pas coupable de ~w.~n', [Suspect, CrimeType])
        ),
        fail  % Force la boucle à continuer
    ;   write('Requête invalide. Essayez à nouveau.'), nl, fail
    )).