:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_cors)).

% Types de crime
crime_type(vol).
crime_type(assassinat).
crime_type(escroquerie).

% --- Faits ---

% Suspects
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% Mobiles (Motive)
has_motive(john, vol).
has_motive(mary, assassinat).
has_motive(alice, escroquerie).

% Présence sur la scène du crime (Near crime scene)
was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).

% Empreintes digitales (Fingerprints)
has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(mary, assassinat).

% Preuves ADN (DNA Evidence)
has_dna_evidence(john, vol).
has_dna_evidence(alice, escroquerie).

% Témoins oculaires (Eyewitness)
eyewitness_identification(mary, assassinat).

% Vidéosurveillance (Camera footage)
has_camera_footage(mary, assassinat).
has_camera_footage(bruno, escroquerie).

% Transactions bancaires (Bank transactions)
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).

% Fausses identités (Fake identities)
owns_fake_identity(sophie, escroquerie).

% Alibis
has_alibi(sophie, escroquerie).


% --- Règles de Culpabilité ---

is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    ( has_fingerprint_on_weapon(Suspect, vol)
    ; has_dna_evidence(Suspect, vol)
    ),
    \+ has_alibi(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)
    ; has_camera_footage(Suspect, assassinat)
    ),
    \+ has_alibi(Suspect, assassinat).

% Règle renforcée pour l'escroquerie : nécessite au moins deux preuves complémentaires
is_guilty(Suspect, escroquerie) :-
    (   (has_motive(Suspect, escroquerie) ; owns_fake_identity(Suspect, escroquerie)),
        (has_bank_transaction(Suspect, escroquerie) ; has_dna_evidence(Suspect, escroquerie) ; has_camera_footage(Suspect, escroquerie))
    ),
    \+ has_alibi(Suspect, escroquerie).


% --- Système de preuves et score de suspicion ---

% Définition des preuves applicables par type de crime
applicable_proof(vol, motive).
applicable_proof(vol, near_scene).
applicable_proof(vol, fingerprint).
applicable_proof(vol, dna).

applicable_proof(assassinat, motive).
applicable_proof(assassinat, near_scene).
applicable_proof(assassinat, fingerprint).
applicable_proof(assassinat, eyewitness).
applicable_proof(assassinat, camera).

applicable_proof(escroquerie, motive).
applicable_proof(escroquerie, bank).
applicable_proof(escroquerie, fake_id).
applicable_proof(escroquerie, dna).
applicable_proof(escroquerie, camera).

% Prédicats de preuve individuels
proof(S, C, motive) :- has_motive(S, C).
proof(S, C, near_scene) :- was_near_crime_scene(S, C).
proof(S, C, fingerprint) :- has_fingerprint_on_weapon(S, C).
proof(S, C, eyewitness) :- eyewitness_identification(S, C).
proof(S, C, dna) :- has_dna_evidence(S, C).
proof(S, C, camera) :- has_camera_footage(S, C).
proof(S, C, bank) :- has_bank_transaction(S, C).
proof(S, C, fake_id) :- owns_fake_identity(S, C).

% Calcul du niveau de suspicion (logique améliorée)
suspicion_level(Suspect, CrimeType, Level) :-
    findall(P, (applicable_proof(CrimeType, P), proof(Suspect, CrimeType, P)), ProofsFound),
    list_to_set(ProofsFound, UniqueProofs),
    length(UniqueProofs, ProofCount),
    findall(P, applicable_proof(CrimeType, P), ApplicableProofs),
    length(ApplicableProofs, MaxProofs),
    (MaxProofs > 0 -> Level is (ProofCount * 100) // MaxProofs ; Level = 0).


% --- Serveur HTTP ---

:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
:- http_handler(root(investigate), handle_investigation, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

handle_investigation(Request) :-
    http_read_json_dict(Request, In),
    atom_string(Suspect, In.suspect),
    atom_string(CrimeType, In.crime),
    
    (is_guilty(Suspect, CrimeType) -> Verdict = true ; Verdict = false),
    suspicion_level(Suspect, CrimeType, Level),
    findall(Proof, (applicable_proof(CrimeType, Proof), proof(Suspect, CrimeType, Proof)), Proofs),
    list_to_set(Proofs, UniqueProofs),

    reply_json_dict(_{verdict: Verdict, suspicionLevel: Level, evidence: UniqueProofs}).

:- set_setting(http:cors, [*]).

main :-
    server(8000),
    format('Serveur lancé sur http://localhost:8000/~n', []).
