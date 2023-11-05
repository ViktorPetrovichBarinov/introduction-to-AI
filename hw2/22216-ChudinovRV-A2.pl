:- prolog_load_context(directory,Dir), atom_concat(Dir, '/wordnet', WNDB_),
    absolute_file_name(WNDB_, WNDB), asserta(user:file_search_path(wndb, WNDB)).
:- use_module('C:/Users/chydi/prolog_tasks/hw2/wnload/prolog/wn').
:- use_module(library(clpfd)).
/*wn_s(Synset, _, Word, PoSTag, Sense, _).
 * Synset - ��� �������
 * Word - ����� �� ����� �������
 * PoSTag - ��� ����� ����
 * Sene - ��� �������� � ������� ������ ����� ������ � ������
 *
 * ���� � ���� ����� ����� ����� ��������� ��������
 * � �������������� ������ � ��������� ��������
 */
/*wn_g(Synset, Description).
 * Synset -  ��� �������
 * Description - �������� ���������������� �������
 */

/*wn_hyp(Synset1, Synset2).
 * �������� �����������
 * ��������� ��������, ��� Synset2 �������� ���������� ����,
 * ������� ����������� � Synset1.
 * �.�. ����� Synset1 - �������� ���������� �������
 *            Synset2 - �������� ����� ������
 */

/*wn_mm(Synset1, Synset2).
 * Synset1 �������� member �������� ���� �� Stmset2.
 * Synset1 - ���������� �������
 * Synset2 - �������
 */

/*wn_mp(Synset1, Synset2)/
 * Synset1 �������� ��������� �������� Synset2
 * Synset1 - ����� (����)
 * Synset2 - ����� (�����)
 */

/*related_words(Word1/PoS1/Sense1/Syn1,
                Word2/PoS2/Sense2/Syn2,
                MaxDist,
                Connection).
 */
/*
 * Connection r(W1/P1/Sen1, Rel, W2/P2/Sen2)
 */

% true, ���� ����������
% false, ���� ����������
% fail � ������ ������ �������� ����� � ��������� �� �������������
connection_exists(_, _, _, Length, _, MaxLength) :- Length #> MaxLength, fail.

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_hyp(Synset1, Synset2),
    not(member((Synset1, hyp, Synset2), Acc)),
    Connection = [(Synset1, hyp, Synset2)|Acc].
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_hyp(Synset2, Synset1),
    not(member((Synset2, hyp, Synset1), Acc)),
    Connection = [(Synset2, hyp, Synset1)|Acc].

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_mm(Synset1, Synset2),
    not(member((Synset1, mm, Synset2), Acc)),
    Connection = [(Synset1, mm, Synset2)|Acc].
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_mm(Synset2, Synset1),
    not(member((Synset2, mm, Synset1), Acc)),
    Connection = [(Synset2, mm, Synset1)|Acc].

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_mp(Synset1, Synset2),
    not(member((Synset1, mp, Synset2), Acc)),
    Connection = [(Synset1, mp, Synset2)|Acc].
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #=< MaxLength,
    wn_mp(Synset2, Synset1),
    not(member((Synset2, mp, Synset1), Acc)),
    Connection = [(Synset2, mp, Synset1)|Acc].
%%%%%%%%%%%%%%%%%%%%%%%%%

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_hyp(Synset1, SynsetConnected),
    not(member((Synset1, hyp, SynsetConnected), Acc)),
    connection_exists(SynsetConnected, Synset2,
                    [(Synset1, hyp, SynsetConnected)|Acc] , Length + 1, Connection, MaxLength).
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_hyp(SynsetConnected, Synset1),
    not(member((SynsetConnected, hyp, Synset1), Acc)),
    connection_exists(SynsetConnected, Synset2,
                      [(SynsetConnected, hyp, Synset1)|Acc], Length + 1, Connection, MaxLength).

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_mm(Synset1, SynsetConnected),
    not(member((Synset1, mm, SynsetConnected), Acc)),
    connection_exists(SynsetConnected, Synset2,
                      [(Synset1, mm, SynsetConnected)| Acc], Length + 1, Connection, MaxLength).
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_mm(SynsetConnected, Synset1),
    not(member((SynsetConnected, mm, Synset1), Acc)),
    connection_exists(SynsetConnected, Synset2,
                      [(SynsetConnected, mm, Synset1)| Acc], Length + 1, Connection, MaxLength).

connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_mp(Synset1, SynsetConnected),
    not(member((Synset1, mp, SynsetConnected), Acc)),
    connection_exists(SynsetConnected, Synset2,
                      [(Synset1, mp, SynsetConnected)| Acc], Length + 1, Connection, MaxLength).
connection_exists(Synset1, Synset2, Acc, Length, Connection, MaxLength) :-
    Length #< MaxLength,
    wn_mp(SynsetConnected, Synset1),
    not(member((SynsetConnected, mp, Synset1), Acc)),
    connection_exists(SynsetConnected, Synset2,
                      [(SynsetConnected, mp, Synset1)| Acc], Length + 1, Connection, MaxLength).

related_words(Word1/PoS1/Sense1/Syn1,
                Word2/PoS2/Sense2/Syn2,
                MaxDist,
                Connection) :-
    wn_s(Syn1, _, Word1, PoS1, Sense1, _),
    wn_s(Syn2, _, Word2, PoS2, Sense2, _),
    connection_exists(Syn1, Syn2, [], 1, Synsets, MaxDist),
    length(Synsets, Len),
    Len #=< MaxDist,
    create_connection(Synsets, [], Connection).

create_connection([(Synset1, Relation, Synset2)], Result, Connection) :-
    wn_s(Synset1, _, Word1, PoS1, Sense1, _),
    wn_s(Synset2, _, Word2, PoS2, Sense2, _),
    Connection = [r(Word1/PoS1/Sense1, Relation, Word2/PoS2/Sense2)|Result].

create_connection([(Synset1, Relation, Synset2)|Ending], Result, Connection) :-
    wn_s(Synset1, _, Word1, PoS1, Sense1, _),
    wn_s(Synset2, _, Word2, PoS2, Sense2, _),
    create_connection(Ending,
                      [r(Word1/PoS1/Sense1, Relation, Word2/PoS2/Sense2)|Result],
                      Connection).
