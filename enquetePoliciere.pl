% Types de crime
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).


% Faits
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).
has_dna_evidence(john, vol). % Nouvelle preuve : ADN

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).
eyewitness_identification(mary, assassinat).
has_camera_footage(mary, assassinat). % Nouvelle preuve : Caméra

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).
has_dna_evidence(alice, escroquerie). % Nouvelle preuve : ADN

has_bank_transaction(bruno, escroquerie).
has_camera_footage(bruno, escroquerie). % Nouvelle preuve : Caméra

owns_fake_identity(sophie, escroquerie).
has_alibi(sophie, escroquerie). % Nouvelle preuve : Alibi

% Règles
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    ( has_fingerprint_on_weapon(Suspect, vol)
    ; has_dna_evidence(Suspect, vol)
    ),
    \+ has_alibi(Suspect, vol). % Vérifie l'absence d'alibi

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ; has_camera_footage(Suspect, assassinat)
    ),
    \+ has_alibi(Suspect, assassinat).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    ( has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ; has_dna_evidence(Suspect, escroquerie)
    ),
    \+ has_alibi(Suspect, escroquerie).

% Calcul du niveau de suspicion (score de 0 à 100)
suspicion_level(Suspect, CrimeType, Level) :-
    findall(Proof, proof(Suspect, CrimeType, Proof), Proofs),
    length(Proofs, ProofCount),
    MaxProofs is 5, % Maximum de preuves possibles
    Level is (ProofCount * 100) // MaxProofs.

proof(Suspect, CrimeType, _) :- has_motive(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- was_near_crime_scene(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- has_fingerprint_on_weapon(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- eyewitness_identification(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- has_dna_evidence(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- has_camera_footage(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- has_bank_transaction(Suspect, CrimeType).
proof(Suspect, CrimeType, _) :- owns_fake_identity(Suspect, CrimeType).

% Historique des enquêtes stockage simple en memoire
:- dynamic investigation_record/3.
record_investigation(Suspect, CrimeType, Verdict) :-
    asserta(investigation_record(Suspect, CrimeType, Verdict)).

% Entrée principale
main :-
    repeat,
    write('Entrez une requête (ex: crime(john, vol)) ou "stop" pour quitter: '), nl,
    read(Input),
    (Input = stop -> ! ; % Condition de sortie
     (crime(Suspect, CrimeType) = Input ->
         (is_guilty(Suspect, CrimeType) ->
             format('~w est coupable de ~w.~n', [Suspect, CrimeType]),
             suspicion_level(Suspect, CrimeType, Level),
             format('Niveau de suspicion: ~w%.~n', [Level]),
             record_investigation(Suspect, CrimeType, 'coupable')
         ;   format('~w n\'est pas coupable de ~w.~n', [Suspect, CrimeType]),
             record_investigation(Suspect, CrimeType, 'non_coupable')
         )
     ;   write('Requête invalide. Essayez à nouveau.'), nl, fail
     ),fail).
    

% Afficher historique des enquêtes
show_history :-
    findall((Suspect, CrimeType, Verdict), investigation_record(Suspect, CrimeType, Verdict), Records),
    (Records \= [] ->
         write('Historique des enquêtes:'), nl,
         maplist(write_record, Records)
    ;   write('Aucun historique disponible.'), nl
    ).

write_record((Suspect, CrimeType, Verdict)) :-
    format('~w - ~w: ~w~n', [Suspect, CrimeType, Verdict]).