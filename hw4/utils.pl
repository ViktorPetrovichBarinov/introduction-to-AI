:- module(utils, [prepare_env/1,
				  have_common_item/2,
				  delete_first_occurrence/3,
				  clean/0,
				  st_group/2,
				  student_follows_both_classes/2,
				  teacher_teaches_both_classes/2,
				  prepared/0,
				  relates_to/2]).

:- use_module(large_data).

:- dynamic st_group/2.
:- dynamic student_follows_both_classes/2.
:- dynamic teacher_teaches_both_classes/2.
:- dynamic prepared/0.


% prepare_env(+Exams)
% 			+Exams: Identifiers of all exams
%
% Sift through the database and assert
% auxiliary predicates which are used a lot later on.
% When prepared the predicate prepared/0 is asserted.
% Does nothing if prepared/0 has already been asserted.
prepare_env(_) :- prepared, !.
prepare_env(Exams) :- prepare(Exams), assert(prepared), !.


% prepare(+Exams)
% 			+Exams: Identifiers of all exams
%
% Prepares runtime environment. The following predicates
% are asserted:
% - st_group/2,
% - teacher_teaches_both_classes/2
% - student_follows_both_classes/2.
%
% Loops through the list of exams and calls have_same_teacher/2 and have_same_student/3
% for each course associated with the current exam.
prepare([]).
prepare([EID | RestExams]) :-
	findall(SID, follows(SID, EID), Students),
	asserta(st_group(EID, Students)),
	have_same_teacher(EID, RestExams),
	have_same_student(EID, Students, RestExams),
	prepare(RestExams).


% have_same_teacher(+CID, +EIDs)
%		+CID: Class ID
%		+EIDs: List of Exam IDs
%
% Checks if the teacher of class CID is
% associated with any of the courses linked
% to the exams in EIDs. If yes, teacher_teaches_both_classes/2
% is asserted.
have_same_teacher(_, []).
have_same_teacher(CID, [OtherEID | RestExams]) :-
	teaches(TID, CID),
	teaches(TID, OtherEID),
	asserta(teacher_teaches_both_classes(CID, OtherEID)),
	asserta(teacher_teaches_both_classes(OtherEID, CID)),
	have_same_teacher(CID, RestExams).
have_same_teacher(CID, [_|RestExams]) :-
	have_same_teacher(CID, RestExams).


% have_same_student(+CID, +Students, +EIDs)
%		+CID: Course ID
%		+Students: Students following this course
%		+EIDs: List of Exam IDs
%
% Checks if any of the Students following course
% with CID are associated with any of the courses
% linked to the exam in EIDs. If yes, student_follows_both_classes/2
% is asserted.
have_same_student(_, _, []).
have_same_student(CID, Students, [OtherEID|RestExams]) :-
	findall(SID, follows(SID, OtherEID), OtherStudents),
	have_common_item(Students, OtherStudents),
	asserta(student_follows_both_classes(CID, OtherEID)),
	asserta(student_follows_both_classes(OtherEID,CID)),
	have_same_student(CID, Students, RestExams).
have_same_student(CID, Students, [_|RestExams]) :-
	have_same_student(CID, Students, RestExams).


% have_common_item(+List1, +List2)
%		+List1: Any list
%		+List2: Any list
%
% Checks if any member of List1 is also a
% member of List2.
have_common_item([], _) :- false.
have_common_item([First|_],OtherList) :-
	member(First, OtherList),
	!.
have_common_item([_|Rest],OtherList) :-
	have_common_item(Rest,OtherList).


% clean
%
% Clear the environment: retract all predicates
% asserted with prepare/1.
clean :-
	retractall(st_group(_,_)),
	retractall(student_follows_both_classes(_,_)),
	retractall(teacher_teaches_both_classes(_,_)),
	retractall(prepared).


% delete_first_occurrence(?Elem, ?List, ?ResultList)
%		?Elem: Any ground term
%		?List: Any List
%		?ResultList: Same as List but without Elem
%
% ResultList is the same as List but with the
% first occurence of Elem removed.
delete_first_occurrence(E,[E|T],T) :- !.
delete_first_occurrence(E,[H|T1],[H|T2]) :-
	delete_first_occurrence(E,T1,T2).


% relates_to(+PID, +EID)
%		+PID: Person ID
%		+EID: ID of exam
%
% Checks whether person with PID is linked to exam
% with EID. If PID is a teacher, PID needs to teach
% class associated with EID. If PID is a student,
% PID needs to follow class associated with EID.
relates_to(PID, EID) :- teacher(PID, _), teaches(PID, EID), !.
relates_to(PID, EID) :- student(PID, _), follows(PID, EID), !.
relates_to(_,_) :- fail.
