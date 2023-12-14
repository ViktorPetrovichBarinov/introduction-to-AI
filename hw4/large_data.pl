:- module(large_data, [
	      teacher/2,
	      student/2,
	      class/2,
	      exam/2,
	      classroom/2,
	      class_has_exam/1,
	      exam_duration/2,
	      follows/2,
	      teaches/2,
	      classroom_capacity/2,
	      ex_season_starts/1,
	      ex_season_ends/1,
	      classroom_available/4,
	      c_lunch_break/2,
	      c_no_exams_in_row/2,
	      c_no_exams_same_day/2,
	      c_no_exam_in_period/5,
	      c_not_in_period/6,
	      c_correction_time/2,
	      c_not_enough_correction_penalty/2,
	      c_study_time/2,
	      c_not_enough_study_penalty/2]).

%Allow grouping constraints per students/lecturers
:- discontiguous
	 c_lunch_break/2,
	 c_not_in_period/6,
	 c_no_exams_same_day/2,
	 c_no_exams_in_row/2.


% Students
student(UUID, Name) :- sp_unit_profile(UUID, Name, student).

% Teachers
teacher(UUID, Name) :- sp_unit_profile(UUID, Name, teacher).

% Exams
exam(ID, Name) :- class_has_exam(ID), class(ID, Name).

% Durations of exams: in this case every exam takes 2 hours
exam_duration("5fc5e4838d0bcf210dbe3774",3).
exam_duration("5fc5e49ced5eeead04eeb2f9",3).
exam_duration("5fc5e4a5b4e8896a9991064d",4).
exam_duration("5fc5e4ad0fd3a0c96e51bb6a",2).
exam_duration("5fc5e4b5415574b82c0d8115",2).
exam_duration("5fc5e4c00b15ca7050a8b045",2).
exam_duration("5fc5e4c8af1d16f6b72abe0c",2).
exam_duration("5fc5e4d5a08e6abc491ec5da",2).
exam_duration("5fc5e4dcc902e9650b60228b",2).
exam_duration("5fc5e4e43557c3e4e8a47bff",2).
exam_duration("5fc5e4ed79b5505f5963e147",2).
exam_duration("5fc5e4f41343a497cffa6f53",2).
exam_duration("5fc5e4fbb89c74d6e93b1c29",2).
exam_duration("5fc5e50303a0d86ae52e066f",2).
exam_duration("5fc5e5095c42bb58a7e4b503",2).
exam_duration("5fc5e5112886ed2aa8037c68",2).
exam_duration("5fc5e51858d6e87fd58449be",2).
exam_duration("5fc5e51fd3f3fb935504ade5",3).
exam_duration("5fc5e5264f08904fd7c87f69",3).
exam_duration("5fc5e52dd58aa03875b9fb2b",4).
exam_duration("5fc5e534b7035630e8ec8292",2).
exam_duration("5fc5e53bfbeb0baecec700ff",2).
exam_duration("5fc5e5422b026960995bb91b",2).
exam_duration("5fc5e5489005cc3067ccb44e",2).
exam_duration("5fc5e550bbc093c82678bb8a",2).
exam_duration("5fc5e56034906550ebbf4e73",2).
exam_duration("5fc5e565b00c34cef303a8d3",2).
exam_duration("5fc5e56ce9b206f06ab7dcc7",2).
exam_duration("5fc5e5724dbd5a8a2b1d8676",2).
exam_duration("5fc5e578437343e26e49fd00",2).
exam_duration("5fc5e57f8ad2cfd4d48ba876",2).
exam_duration("5fc5e585c940f00a5c7631e2",2).
exam_duration("5fc5e58acdfef25a6d495a93",2).
exam_duration("5fc5e59329756063c0560816",2).

% Room capacity
classroom_capacity(3258,10).
classroom_capacity(3318,30).
classroom_capacity(3107,50).

% Availability of rooms
classroom_available(Room, Day, 9, 17) :- 
	classroom(Room, _),
	(between(3,7,Day);between(10,14,Day);between(17,21,Day)).

% The exam season runs from the first_day to the last_day
ex_season_starts(1).
ex_season_ends(23).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSTRAINTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% How many correction time every exam requires
c_correction_time("5fc5e4838d0bcf210dbe3774",6).
c_correction_time("5fc5e49ced5eeead04eeb2f9",4).
c_correction_time("5fc5e4a5b4e8896a9991064d",5).
c_correction_time("5fc5e4ad0fd3a0c96e51bb6a",2).
c_correction_time("5fc5e4b5415574b82c0d8115",4).
c_correction_time("5fc5e4c00b15ca7050a8b045",2).
c_correction_time("5fc5e4c8af1d16f6b72abe0c",2).
c_correction_time("5fc5e4d5a08e6abc491ec5da",4).
c_correction_time("5fc5e4dcc902e9650b60228b",2).
c_correction_time("5fc5e4e43557c3e4e8a47bff",4).
c_correction_time("5fc5e4ed79b5505f5963e147",2).
c_correction_time("5fc5e4f41343a497cffa6f53",2).
c_correction_time("5fc5e4fbb89c74d6e93b1c29",4).
c_correction_time("5fc5e50303a0d86ae52e066f",1).
c_correction_time("5fc5e5095c42bb58a7e4b503",1).
c_correction_time("5fc5e5112886ed2aa8037c68",1).
c_correction_time("5fc5e51858d6e87fd58449be",1).
c_correction_time("5fc5e51fd3f3fb935504ade5",6).
c_correction_time("5fc5e5264f08904fd7c87f69",5).
c_correction_time("5fc5e52dd58aa03875b9fb2b",3).
c_correction_time("5fc5e534b7035630e8ec8292",2).
c_correction_time("5fc5e53bfbeb0baecec700ff",2).
c_correction_time("5fc5e5422b026960995bb91b",2).
c_correction_time("5fc5e5489005cc3067ccb44e",1).
c_correction_time("5fc5e550bbc093c82678bb8a",1).
c_correction_time("5fc5e56034906550ebbf4e73",1).
c_correction_time("5fc5e565b00c34cef303a8d3",1).
c_correction_time("5fc5e56ce9b206f06ab7dcc7",4).
c_correction_time("5fc5e5724dbd5a8a2b1d8676",4).
c_correction_time("5fc5e578437343e26e49fd00",2).
c_correction_time("5fc5e57f8ad2cfd4d48ba876",2).
c_correction_time("5fc5e585c940f00a5c7631e2",2).
c_correction_time("5fc5e58acdfef25a6d495a93",2).
c_correction_time("5fc5e59329756063c0560816",2).

% How many study time exams require
c_study_time("5fc5e4838d0bcf210dbe3774",3).
c_study_time("5fc5e49ced5eeead04eeb2f9",3).
c_study_time("5fc5e4a5b4e8896a9991064d",5).
c_study_time("5fc5e4ad0fd3a0c96e51bb6a",2).
c_study_time("5fc5e4b5415574b82c0d8115",2).
c_study_time("5fc5e4c00b15ca7050a8b045",2).
c_study_time("5fc5e4c8af1d16f6b72abe0c",2).
c_study_time("5fc5e4d5a08e6abc491ec5da",2).
c_study_time("5fc5e4dcc902e9650b60228b",2).
c_study_time("5fc5e4e43557c3e4e8a47bff",2).
c_study_time("5fc5e4ed79b5505f5963e147",2).
c_study_time("5fc5e4f41343a497cffa6f53",2).
c_study_time("5fc5e4fbb89c74d6e93b1c29",2).
c_study_time("5fc5e50303a0d86ae52e066f",2).
c_study_time("5fc5e5095c42bb58a7e4b503",2).
c_study_time("5fc5e5112886ed2aa8037c68",2).
c_study_time("5fc5e51858d6e87fd58449be",2).
c_study_time("5fc5e51fd3f3fb935504ade5",3).
c_study_time("5fc5e5264f08904fd7c87f69",3).
c_study_time("5fc5e52dd58aa03875b9fb2b",5).
c_study_time("5fc5e534b7035630e8ec8292",2).
c_study_time("5fc5e53bfbeb0baecec700ff",2).
c_study_time("5fc5e5422b026960995bb91b",2).
c_study_time("5fc5e5489005cc3067ccb44e",2).
c_study_time("5fc5e550bbc093c82678bb8a",2).
c_study_time("5fc5e56034906550ebbf4e73",2).
c_study_time("5fc5e565b00c34cef303a8d3",2).
c_study_time("5fc5e56ce9b206f06ab7dcc7",2).
c_study_time("5fc5e5724dbd5a8a2b1d8676",2).
c_study_time("5fc5e578437343e26e49fd00",2).
c_study_time("5fc5e57f8ad2cfd4d48ba876",2).
c_study_time("5fc5e585c940f00a5c7631e2",2).
c_study_time("5fc5e58acdfef25a6d495a93",2).
c_study_time("5fc5e59329756063c0560816",2).

% Teachers preferences

% All teachers prefer a lunch break (12 - 13)
c_lunch_break(Teacher, 1) :- teacher(Teacher, _).

% All teachers prefer not to have exams in a row
c_no_exams_in_row(Teacher, 2) :- teacher(Teacher, _).

% All teachers prefer to be guaranteed to have enough correction time
c_not_enough_correction_penalty(Teacher, 2) :- teacher(Teacher, _).

% c_not_in_period(+Teacher, +Exam, +Day, +From +To, +Cost)
%			+Teacher: teacher ID
%			+Exam: exam ID
%			+Day: day the exam should not fall on between From hour to To hour
%			+From: starting hour
%			+To: ending hour
%			+Cost: penalty for violating this constraint
%
% Some exams shouldn't fall on the last days of the exam period.
% That is teacher with id=Teacher prefer not to have the exam with id=Exam
% on the day=Day between From and To hours. If From=0 and To=24 it means
% the exam should not fall on the day at all.
c_not_in_period("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4","5fc5e4838d0bcf210dbe3774",21,0,24,10).
c_not_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e","5fc5e51fd3f3fb935504ade5",21,0,24,10).
c_not_in_period("91e54a7a-339e-11eb-b04b-9f53fc5784a4","5fc5e4a5b4e8896a9991064d",21,0,24,5).
c_not_in_period("91e65b22-339e-11eb-8505-6ff5479b62df","5fc5e5264f08904fd7c87f69",21,0,24,5).
c_not_in_period("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4","5fc5e4838d0bcf210dbe3774",20,0,24,5).
c_not_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e","5fc5e51fd3f3fb935504ade5",20,0,24,5).

% Some teachers prefer not to have exams in a period
c_no_exam_in_period("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4",10,0,24,5). %Janis Bolls prefers to have no exam day 10.
c_no_exam_in_period("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4",18,0,12,2). %Janis Bolls prefers to have no exam before noon, day 18.
c_no_exam_in_period("91e54a7a-339e-11eb-8f3c-bf626a7840c3",7,0,24,5). %Ana Falacco prefers to have no exam day 7.
c_no_exam_in_period("91e54a7a-339e-11eb-8f3c-bf626a7840c3",11,0,12,5). %Ana Falacco prefers to have no exam before noon, day 11.
c_no_exam_in_period("91e54a7a-339e-11eb-8f3c-bf626a7840c3",17,12,24,2). %Ana Falacco prefers to have no exam in the afternoon, day 17.
c_no_exam_in_period("91e54a7a-339e-11eb-8f3c-bf626a7840c3",21,0,12,5). %Ana Falacco prefers to have no exam before noon, day 21.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",5,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 5.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",12,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 12.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",19,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 19.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",6,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 6.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",13,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 13.
c_no_exam_in_period("91e57162-339e-11eb-9aaf-0f3c50044e0c",20,12,24,2). %Glenn Janson prefers to have no exam in the afternoon, day 20.
c_no_exam_in_period("91e59840-339e-11eb-a0de-4bded3fecf68",6,12,24,5). %Hong Fedde prefers to have no exam in the afternoon, day 6.
c_no_exam_in_period("91e59840-339e-11eb-a0de-4bded3fecf68",12,0,12,5). %Hong Fedde prefers to have no exam before noon, day 12.
c_no_exam_in_period("91e59840-339e-11eb-a0de-4bded3fecf68",14,0,24,5). %Hong Fedde prefers to have no exam day 14.
c_no_exam_in_period("91e5bf3c-339e-11eb-9803-ef6ca062a35c",3,0,24,2). %Nicky Waltos prefers to have no exam day 3.
c_no_exam_in_period("91e5bf3c-339e-11eb-9803-ef6ca062a35c",4,0,24,2). %Nicky Waltos prefers to have no exam day 4.
c_no_exam_in_period("91e5bf3c-339e-11eb-9803-ef6ca062a35c",5,0,24,2). %Nicky Waltos prefers to have no exam day 5.
c_no_exam_in_period("91e5bf3c-339e-11eb-9803-ef6ca062a35c",6,0,24,2). %Nicky Waltos prefers to have no exam day 6.
c_no_exam_in_period("91e5bf3c-339e-11eb-9803-ef6ca062a35c",7,0,24,2). %Nicky Waltos prefers to have no exam day 7.
c_no_exam_in_period("91e5e638-339e-11eb-be34-0bbe6a876006",17,0,24,2). %Shae Mostowy prefers to have no exam day 17.
c_no_exam_in_period("91e5e638-339e-11eb-be34-0bbe6a876006",18,0,24,2). %Shae Mostowy prefers to have no exam day 18.
c_no_exam_in_period("91e5e638-339e-11eb-be34-0bbe6a876006",19,0,24,2). %Shae Mostowy prefers to have no exam day 19.
c_no_exam_in_period("91e5e638-339e-11eb-be34-0bbe6a876006",20,0,24,2). %Shae Mostowy prefers to have no exam day 20.
c_no_exam_in_period("91e5e638-339e-11eb-be34-0bbe6a876006",21,0,24,2). %Shae Mostowy prefers to have no exam day 21.
c_no_exam_in_period("91e63444-339e-11eb-ac9a-3fc51fa5840d",10,0,24,5). %Delmer Zwilling prefers to have no exam day 10.
c_no_exam_in_period("91e63444-339e-11eb-ac9a-3fc51fa5840d",11,0,24,5). %Delmer Zwilling prefers to have no exam day 11.
c_no_exam_in_period("91e63444-339e-11eb-ac9a-3fc51fa5840d",12,0,24,5). %Delmer Zwilling prefers to have no exam day 12.
c_no_exam_in_period("91e63444-339e-11eb-ac9a-3fc51fa5840d",13,0,24,5). %Delmer Zwilling prefers to have no exam day 13.
c_no_exam_in_period("91e63444-339e-11eb-ac9a-3fc51fa5840d",14,0,24,5). %Delmer Zwilling prefers to have no exam day 14.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",6,0,12,5). %Shalon Stains prefers to have no exam before noon, day 6.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",13,0,12,5). %Shalon Stains prefers to have no exam before noon, day 13.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",20,0,12,5). %Shalon Stains prefers to have no exam before noon, day 20.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",7,0,12,5). %Shalon Stains prefers to have no exam before noon, day 7.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",14,0,12,5). %Shalon Stains prefers to have no exam before noon, day 14.
c_no_exam_in_period("91e63444-339e-11eb-8354-e7cb5b5b888e",21,0,12,5). %Shalon Stains prefers to have no exam before noon, day 21.
c_no_exam_in_period("91e65b22-339e-11eb-8505-6ff5479b62df",6,12,24,5). %Colleen Raike prefers to have no exam in the afternoon, day 6.
c_no_exam_in_period("91e65b22-339e-11eb-8505-6ff5479b62df",18,0,12,2). %Colleen Raike prefers to have no exam before noon, day 18.
c_no_exam_in_period("91e65b22-339e-11eb-8505-6ff5479b62df",19,0,12,5). %Colleen Raike prefers to have no exam before noon, day 19.
c_no_exam_in_period("91e65b22-339e-11eb-8505-6ff5479b62df",21,12,24,2). %Colleen Raike prefers to have no exam in the afternoon, day 21.
c_no_exam_in_period("91e6820a-339e-11eb-9470-1b2eae064281",10,12,24,2). %Oswaldo Bardo prefers to have no exam in the afternoon, day 10.
c_no_exam_in_period("91e6a910-339e-11eb-a502-5f4651b13629",5,0,12,5). %Jama Nguyn prefers to have no exam before noon, day 5.
c_no_exam_in_period("91e6a910-339e-11eb-a502-5f4651b13629",12,0,12,5). %Jama Nguyn prefers to have no exam before noon, day 12.
c_no_exam_in_period("91e6a910-339e-11eb-a502-5f4651b13629",19,0,12,5). %Jama Nguyn prefers to have no exam before noon, day 19.
c_no_exam_in_period("91e6cee0-339e-11eb-8791-7b2ecfcb57da",5,0,12,5). %Charleen Mellema prefers to have no exam before noon, day 5.
c_no_exam_in_period("91e6f6ea-339e-11eb-b667-3fd4d063c2ee",Day,15,24,3) :- 
	ex_season_starts(Start), ex_season_ends(End), between(Start,End,Day). %Adelina Gord prefers to have no exam after 15h.
c_no_exam_in_period("91e6f6ea-339e-11eb-8d6b-a79bf3e78050",6,0,12,5). %Madeleine Ryba prefers to have no exam before noon, day 6.
c_no_exam_in_period("91e6f6ea-339e-11eb-8d6b-a79bf3e78050",13,0,12,5). %Madeleine Ryba prefers to have no exam before noon, day 13.
c_no_exam_in_period("91e6f6ea-339e-11eb-8d6b-a79bf3e78050",20,0,12,5). %Madeleine Ryba prefers to have no exam before noon, day 20.

% Students preferences

% All students prefer a lunch break
c_lunch_break(Student, 1) :- student(Student, _).

% All students prefer not to have multiple exams on the same day
c_no_exams_same_day(Student, 3) :- student(Student, _).

% All students prefer not to have exams in a row
c_no_exams_in_row(Student, 5) :- student(Student, _).

% Every student prefers to be guaranteed to have enough study time
c_not_enough_study_penalty(Student, 2) :- student(Student, _).

% Some exams shouldn't fall on the first days of the exam period
c_not_in_period(S,"5fc5e4a5b4e8896a9991064d",3,0,24,8) :- student(S,_), follows(S,"5fc5e4a5b4e8896a9991064d").
c_not_in_period(S,"5fc5e5264f08904fd7c87f69",3,0,24,8) :- student(S,_), follows(S,"5fc5e5264f08904fd7c87f69").
c_not_in_period(S,"5fc5e4a5b4e8896a9991064d",4,0,24,4) :- student(S,_), follows(S,"5fc5e4a5b4e8896a9991064d").
c_not_in_period(S,"5fc5e5264f08904fd7c87f69",4,0,24,4) :- student(S,_), follows(S,"5fc5e5264f08904fd7c87f69").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% List of classes
%
class("5fc5e4838d0bcf210dbe3774", "Math 1.0").
class("5fc5e49ced5eeead04eeb2f9", "Math 2.0").
class("5fc5e4a5b4e8896a9991064d", "Advanced Math 2.0").
class("5fc5e4ad0fd3a0c96e51bb6a", "Religion 1.0").
class("5fc5e4b5415574b82c0d8115", "Religion 2.0").
class("5fc5e4c00b15ca7050a8b045", "Philosophy 1.0").
class("5fc5e4c8af1d16f6b72abe0c", "Philosophy 2.0").
class("5fc5e4d5a08e6abc491ec5da", "History 1.0").
class("5fc5e4dcc902e9650b60228b", "History 2.0").
class("5fc5e4e43557c3e4e8a47bff", "Socio-Economic Initiation").
class("5fc5e4ed79b5505f5963e147", "Politics & Sociology").
class("5fc5e4f41343a497cffa6f53", "Psychology").
class("5fc5e4fbb89c74d6e93b1c29", "Art Initiation").
class("5fc5e50303a0d86ae52e066f", "Art History").
class("5fc5e5095c42bb58a7e4b503", "Architecture").
class("5fc5e5112886ed2aa8037c68", "Painting & Sculpture").
class("5fc5e51858d6e87fd58449be", "Music & Performing Art").
class("5fc5e51fd3f3fb935504ade5", "English 1.0").
class("5fc5e5264f08904fd7c87f69", "English 2.0").
class("5fc5e52dd58aa03875b9fb2b", "Advanced English 2.0").
class("5fc5e534b7035630e8ec8292", "French").
class("5fc5e53bfbeb0baecec700ff", "German").
class("5fc5e5422b026960995bb91b", "Dutch").
class("5fc5e5489005cc3067ccb44e", "Spanish").
class("5fc5e550bbc093c82678bb8a", "Russian").
class("5fc5e56034906550ebbf4e73", "Chinese").
class("5fc5e565b00c34cef303a8d3", "Latin").
class("5fc5e56ce9b206f06ab7dcc7", "Information & Communication Technology").
class("5fc5e5724dbd5a8a2b1d8676", "Science Initiation").
class("5fc5e578437343e26e49fd00", "Biology").
class("5fc5e57f8ad2cfd4d48ba876", "Physics").
class("5fc5e585c940f00a5c7631e2", "Chemistry").
class("5fc5e58acdfef25a6d495a93", "Informatics").
class("5fc5e59329756063c0560816", "Economy").


%
% Exams
%
class_has_exam("5fc5e4838d0bcf210dbe3774").
class_has_exam("5fc5e49ced5eeead04eeb2f9").
class_has_exam("5fc5e4a5b4e8896a9991064d").
class_has_exam("5fc5e4ad0fd3a0c96e51bb6a").
class_has_exam("5fc5e4b5415574b82c0d8115").
class_has_exam("5fc5e4c00b15ca7050a8b045").
class_has_exam("5fc5e4c8af1d16f6b72abe0c").
class_has_exam("5fc5e4d5a08e6abc491ec5da").
class_has_exam("5fc5e4dcc902e9650b60228b").
class_has_exam("5fc5e4e43557c3e4e8a47bff").
class_has_exam("5fc5e4ed79b5505f5963e147").
class_has_exam("5fc5e4f41343a497cffa6f53").
class_has_exam("5fc5e4fbb89c74d6e93b1c29").
class_has_exam("5fc5e50303a0d86ae52e066f").
class_has_exam("5fc5e5095c42bb58a7e4b503").
class_has_exam("5fc5e5112886ed2aa8037c68").
class_has_exam("5fc5e51858d6e87fd58449be").
class_has_exam("5fc5e51fd3f3fb935504ade5").
class_has_exam("5fc5e5264f08904fd7c87f69").
class_has_exam("5fc5e52dd58aa03875b9fb2b").
class_has_exam("5fc5e534b7035630e8ec8292").
class_has_exam("5fc5e53bfbeb0baecec700ff").
class_has_exam("5fc5e5422b026960995bb91b").
class_has_exam("5fc5e5489005cc3067ccb44e").
class_has_exam("5fc5e550bbc093c82678bb8a").
class_has_exam("5fc5e56034906550ebbf4e73").
class_has_exam("5fc5e565b00c34cef303a8d3").
class_has_exam("5fc5e56ce9b206f06ab7dcc7").
class_has_exam("5fc5e5724dbd5a8a2b1d8676").
class_has_exam("5fc5e578437343e26e49fd00").
class_has_exam("5fc5e57f8ad2cfd4d48ba876").
class_has_exam("5fc5e585c940f00a5c7631e2").
class_has_exam("5fc5e58acdfef25a6d495a93").
class_has_exam("5fc5e59329756063c0560816").


%
% Available classrooms
%
classroom(3258, "Room 3258"). % Small classroom
classroom(3318, "Room 3318"). % Normal classroom
classroom(3107, "Room 3107"). % Large classroom


%
% People database
%
sp_unit_profile("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4", "Janis Bolls", teacher).
sp_unit_profile("91e54a7a-339e-11eb-8f3c-bf626a7840c3", "Ana Falacco", teacher).
sp_unit_profile("91e54a7a-339e-11eb-b04b-9f53fc5784a4", "Morgan Nosek", teacher).
sp_unit_profile("91e57162-339e-11eb-9aaf-0f3c50044e0c", "Glenn Janson", teacher).
sp_unit_profile("91e59840-339e-11eb-bd8d-8329ca2a1492", "Bethel Kievit", teacher).
sp_unit_profile("91e59840-339e-11eb-a0de-4bded3fecf68", "Hong Fedde", teacher).
sp_unit_profile("91e5bf3c-339e-11eb-9803-ef6ca062a35c", "Nicky Waltos", teacher).
sp_unit_profile("91e5e638-339e-11eb-8238-ff370afe8c9d", "Zulma Derwin", teacher).
sp_unit_profile("91e5e638-339e-11eb-be34-0bbe6a876006", "Shae Mostowy", teacher).
sp_unit_profile("91e60d20-339e-11eb-bfb9-df974e49905e", "Faye Bakhshian", teacher).
sp_unit_profile("91e63444-339e-11eb-ac9a-3fc51fa5840d", "Delmer Zwilling", teacher).
sp_unit_profile("91e63444-339e-11eb-8354-e7cb5b5b888e", "Shalon Stains", teacher).
sp_unit_profile("91e65b22-339e-11eb-8505-6ff5479b62df", "Colleen Raike", teacher).
sp_unit_profile("91e6820a-339e-11eb-b0d8-bfe9168350bd", "Wanda Griffins", teacher).
sp_unit_profile("91e6820a-339e-11eb-9470-1b2eae064281", "Oswaldo Bardo", teacher).
sp_unit_profile("91e6a910-339e-11eb-a502-5f4651b13629", "Jama Nguyn", teacher).
sp_unit_profile("91e6cee0-339e-11eb-8791-7b2ecfcb57da", "Charleen Mellema", teacher).
sp_unit_profile("91e6f6ea-339e-11eb-b667-3fd4d063c2ee", "Adelina Gord", teacher).
sp_unit_profile("91e6f6ea-339e-11eb-8d6b-a79bf3e78050", "Madeleine Ryba", teacher).

sp_unit_profile("64187cf0-339b-11eb-83ee-578bccab851e", "Cythia Serpe", student).
sp_unit_profile("6418a3ec-339b-11eb-84fa-3ff9d6404d5f", "Christeen Friedli", student).
sp_unit_profile("6418cc14-339b-11eb-a8e7-0f7974624d87", "Kacie Faragoza", student).
sp_unit_profile("6418cc14-339b-11eb-96d9-3fa2ca4a45d4", "Yuonne Cormany", student).
sp_unit_profile("6418f31a-339b-11eb-8c5d-0bd22fc07c59", "Marilu Brauner", student).
sp_unit_profile("64191b24-339b-11eb-8574-274d853701cc", "Myrl Weader", student).
sp_unit_profile("64191b24-339b-11eb-a30f-eb57f891bc68", "Jani Sinks", student).
sp_unit_profile("64194220-339b-11eb-8e1a-bf1891ab09b4", "Wyatt Enriguez", student).
sp_unit_profile("64196912-339b-11eb-9bab-8f8eb7ec2154", "Lamont Vanduyn", student).
sp_unit_profile("64196912-339b-11eb-814c-9f6edcae850b", "Shawanda Vanwyngaarden", student).
sp_unit_profile("6419900e-339b-11eb-9a1d-c315bd6d2984", "Shizue Utecht", student).
sp_unit_profile("6419b700-339b-11eb-92fd-3722c3919b00", "Demarcus Mervyn", student).
sp_unit_profile("6419b700-339b-11eb-9f52-efcef6eaaab9", "Charline Predom", student).
sp_unit_profile("6419ddde-339b-11eb-a9d4-fff5cbb496b0", "Yukiko Chaffins", student).
sp_unit_profile("641a053e-339b-11eb-be81-8335d8c0acc9", "Reynaldo Portlock", student).
sp_unit_profile("641a053e-339b-11eb-b744-4bf524ef3244", "Cory Sauerbry", student).
sp_unit_profile("641a2abe-339b-11eb-8c1c-efce54bb2f93", "Esta Ciraulo", student).
sp_unit_profile("641a52dc-339b-11eb-9fb1-0392c000ed25", "Nicky Testolin", student).
sp_unit_profile("641a52dc-339b-11eb-89fc-17df5c86ebab", "Eliana Updike", student).
sp_unit_profile("641a79ce-339b-11eb-9ea4-3b33e1eca2b3", "Larisa Bachicha", student).
sp_unit_profile("641aa0c0-339b-11eb-9b48-131888dee1cf", "Wesley Mederios", student).
sp_unit_profile("641ac7bc-339b-11eb-9c68-ff323341e980", "Orpha Caballes", student).
sp_unit_profile("641ac7bc-339b-11eb-9f5b-3340438c7a61", "Agripina Husak", student).
sp_unit_profile("641aeeae-339b-11eb-902b-0398cbf76a48", "Jerrica Getzschman", student).
sp_unit_profile("641aeeae-339b-11eb-8802-079e39f1f05f", "Britni Storks", student).
sp_unit_profile("641b1582-339b-11eb-b6b0-5323f4c03010", "Dimple Cayetano", student).
sp_unit_profile("641b3c92-339b-11eb-a059-73e8b65d448c", "Sydney Whitbeck", student).
sp_unit_profile("641b3c92-339b-11eb-9ac9-bff7de771a78", "Kaylee Palmateer", student).
sp_unit_profile("641b638e-339b-11eb-9995-d377ee365170", "Hope Lovick", student).
sp_unit_profile("641b8a80-339b-11eb-9176-0bf9c2c6f510", "Bernarda Rosul", student).
sp_unit_profile("641b8a80-339b-11eb-ad54-8bcbdfdaefc4", "Enriqueta Begeman", student).
sp_unit_profile("641bb172-339b-11eb-a56c-f73126eed4a0", "Landon Plancarte", student).
sp_unit_profile("641bd882-339b-11eb-941d-230e45752110", "Bonny Schwarm", student).
sp_unit_profile("641bd882-339b-11eb-ab0e-375dddd4fb29", "Joel Tiznado", student).
sp_unit_profile("641bff56-339b-11eb-ab5f-47b7a6886f0b", "Echo Chillemi", student).
sp_unit_profile("641c2652-339b-11eb-9d2d-8368da738a8e", "Ernestine Kadish", student).
sp_unit_profile("641c2652-339b-11eb-a62a-afa8247997cd", "Katelin Realmuto", student).
sp_unit_profile("641c4d4e-339b-11eb-bd92-6f7b011dc689", "Erick Parman", student).
sp_unit_profile("641c7440-339b-11eb-9e2f-d7c4172df0b2", "Sammy Qualey", student).
sp_unit_profile("641c9a1a-339b-11eb-a1ee-8f889806076d", "Linh Koppang", student).
sp_unit_profile("641c9a1a-339b-11eb-97d8-3fe8fbdb978d", "Doris Honnen", student).
sp_unit_profile("641cc10c-339b-11eb-a741-c7ea85a5f4fd", "Agripina Shafto", student).
sp_unit_profile("641ce7fe-339b-11eb-8851-c32359247cea", "Aleshia Deines", student).
sp_unit_profile("641ce7fe-339b-11eb-9bc4-572b5e786fbc", "Lia Bandasak", student).
sp_unit_profile("641d1030-339b-11eb-a21b-b3b71142377e", "Lindsey Keens", student).
sp_unit_profile("641d370e-339b-11eb-bf6a-577a9d353e42", "Wilmer Snerling", student).
sp_unit_profile("641d370e-339b-11eb-a829-37809ea78594", "Nelia Huie", student).
sp_unit_profile("641d5e1e-339b-11eb-b613-53b5d7feea1a", "Corliss Jerman", student).
sp_unit_profile("641d8646-339b-11eb-9c8f-37624926dd97", "Margurite Hatake", student).
sp_unit_profile("641d8646-339b-11eb-be4c-df1ee7a7f071", "Eboni Harmeyer", student).
sp_unit_profile("641dad2e-339b-11eb-8acb-e3c82f521e7a", "Richelle Cavaco", student).
sp_unit_profile("641dd420-339b-11eb-9c40-abd2d1f0e135", "Tommie Stipp", student).
sp_unit_profile("641dd420-339b-11eb-bf1f-c755c01eb5ba", "Olen Verkuilen", student).
sp_unit_profile("641dfb30-339b-11eb-9994-d39811fb5f49", "Sun Hase", student).
sp_unit_profile("641e220e-339b-11eb-a178-43e209eb16e9", "Robbie Hoschek", student).
sp_unit_profile("641e220e-339b-11eb-a226-6310cc33a3e5", "Chara Coatsworth", student).
sp_unit_profile("641e4950-339b-11eb-aef1-8b13b7ac7249", "Breann Ozburn", student).
sp_unit_profile("641e6ff2-339b-11eb-bd87-17f98c15ee73", "Jann Mcelhany", student).
sp_unit_profile("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5", "Mandy Wharff", student).
sp_unit_profile("641e969e-339b-11eb-ad2f-27a493e23f23", "Gregory Zarling", student).
sp_unit_profile("641ebde0-339b-11eb-b58b-a7e66a0d591c", "Lavelle Benage", student).
sp_unit_profile("641ebde0-339b-11eb-9887-a752c9637f99", "Annice Booth", student).
sp_unit_profile("641eea04-339b-11eb-991d-37fc01de0af7", "Hannelore Stehly", student).
sp_unit_profile("641f0c46-339b-11eb-a1b8-0fed0cbb4f01", "Dillon Russe", student).
sp_unit_profile("641f32d4-339b-11eb-9cd4-f7bc245d1c70", "Ricky Neblock", student).
sp_unit_profile("641f32d4-339b-11eb-b3b2-a3a73bc44170", "Merrilee Tortelli", student).
sp_unit_profile("641f59c6-339b-11eb-8ca1-cf9bdd97cc01", "Sade Jenks", student).
sp_unit_profile("641f7f8c-339b-11eb-a510-0f88f15e37ce", "Enriqueta Haro", student).
sp_unit_profile("641f7f8c-339b-11eb-828a-cf225f289f3c", "Warren Polian", student).
sp_unit_profile("641fa688-339b-11eb-a09c-c795c334402c", "Marybeth Jellison", student).
sp_unit_profile("641fcd7a-339b-11eb-bdc5-139e1799cfa8", "Salena Keidong", student).
sp_unit_profile("641fcd7a-339b-11eb-97a9-87c213b41bd5", "Ilse Fontneau", student).
sp_unit_profile("641ff598-339b-11eb-829b-bf4cc9d53b81", "Annamaria Ganoung", student).
sp_unit_profile("64201cb2-339b-11eb-a79f-6fc93b31deb8", "Haydee Burbano", student).
sp_unit_profile("64204372-339b-11eb-88d5-f359a213717b", "Angella Yundt", student).
sp_unit_profile("64204372-339b-11eb-be6a-8b86e81ee4a2", "Lenna Briski", student).
sp_unit_profile("64206ba4-339b-11eb-b822-c3750880c936", "Garland Zakrajsek", student).
sp_unit_profile("642092a0-339b-11eb-a19b-f31171bb6e48", "Geri Thoran", student).
sp_unit_profile("642092a0-339b-11eb-8ddd-6fd8c8cd673f", "Carola Hackbart", student).
sp_unit_profile("6420b99c-339b-11eb-8015-833111372de0", "Andrea Nwadiora", student).
sp_unit_profile("6420e1b0-339b-11eb-811b-8756ee8ebe55", "Elease Weser", student).
sp_unit_profile("6420e1b0-339b-11eb-afba-2bf409d54f4e", "Cynthia Antrobus", student).
sp_unit_profile("64210898-339b-11eb-8a56-c7c6e18741da", "Alberto Sumners", student).
sp_unit_profile("64212fe4-339b-11eb-976f-0b2d79b3ac50", "Wilfredo Bassil", student).
sp_unit_profile("64215686-339b-11eb-93fc-a33d34907ac6", "Kattie Kierzewski", student).
sp_unit_profile("64215686-339b-11eb-b851-db350caa1621", "Adolfo Mavai", student).
sp_unit_profile("64217da0-339b-11eb-848d-4f355031aaaf", "Eugenie Rigatti", student).
sp_unit_profile("6421a474-339b-11eb-8414-87ec1119a40f", "Shela Brisker", student).
sp_unit_profile("6421a474-339b-11eb-9732-3307eb5ccaa9", "Delena Groholski", student).
sp_unit_profile("6421ca4e-339b-11eb-9857-3f0cbc8ef02e", "Inge Kassin", student).
sp_unit_profile("6421f262-339b-11eb-bd4c-7bd3cc0b0a76", "Jerry Shinney", student).
sp_unit_profile("64221846-339b-11eb-8a6d-4f73323a093e", "Sueann Mitchem", student).
sp_unit_profile("64221846-339b-11eb-8073-9f7280b9039a", "Cody Baadsgaard", student).
sp_unit_profile("64224172-339b-11eb-b675-67a0f9b5992b", "Calandra Berninger", student).
sp_unit_profile("64226878-339b-11eb-b1d1-6386818a5367", "Maud Halwood", student).
sp_unit_profile("64226878-339b-11eb-b444-835920aaaaff", "Melina Hallford", student).
sp_unit_profile("64228f6a-339b-11eb-94a9-e37ea60c3823", "Alline Kluck", student).
sp_unit_profile("6422b666-339b-11eb-900b-c76c27083730", "Amira Trabucco", student).
sp_unit_profile("6422b666-339b-11eb-941c-9363325fb5cb", "Irena Quivers", student).
sp_unit_profile("6422dd58-339b-11eb-b3fa-e3ee10890d13", "Dorene Winsley", student).

% Teacher-class correspondence table
teaches("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4","5fc5e4838d0bcf210dbe3774").
teaches("91e54a7a-339e-11eb-8f3c-bf626a7840c3","5fc5e49ced5eeead04eeb2f9").
teaches("91e54a7a-339e-11eb-b04b-9f53fc5784a4","5fc5e4a5b4e8896a9991064d").
teaches("91e57162-339e-11eb-9aaf-0f3c50044e0c","5fc5e4ad0fd3a0c96e51bb6a").
teaches("91e57162-339e-11eb-9aaf-0f3c50044e0c","5fc5e4b5415574b82c0d8115").
teaches("91e59840-339e-11eb-bd8d-8329ca2a1492","5fc5e4c00b15ca7050a8b045").
teaches("91e59840-339e-11eb-bd8d-8329ca2a1492","5fc5e4c8af1d16f6b72abe0c").
teaches("91e59840-339e-11eb-a0de-4bded3fecf68","5fc5e4d5a08e6abc491ec5da").
teaches("91e59840-339e-11eb-a0de-4bded3fecf68","5fc5e4dcc902e9650b60228b").
teaches("91e5bf3c-339e-11eb-9803-ef6ca062a35c","5fc5e4e43557c3e4e8a47bff").
teaches("91e5bf3c-339e-11eb-9803-ef6ca062a35c","5fc5e4ed79b5505f5963e147").
teaches("91e5e638-339e-11eb-8238-ff370afe8c9d","5fc5e4f41343a497cffa6f53").
teaches("91e5e638-339e-11eb-be34-0bbe6a876006","5fc5e4fbb89c74d6e93b1c29").
teaches("91e59840-339e-11eb-a0de-4bded3fecf68","5fc5e50303a0d86ae52e066f").
teaches("91e60d20-339e-11eb-bfb9-df974e49905e","5fc5e5095c42bb58a7e4b503").
teaches("91e5e638-339e-11eb-be34-0bbe6a876006","5fc5e5112886ed2aa8037c68").
teaches("91e63444-339e-11eb-ac9a-3fc51fa5840d","5fc5e51858d6e87fd58449be").
teaches("91e63444-339e-11eb-8354-e7cb5b5b888e","5fc5e51fd3f3fb935504ade5").
teaches("91e65b22-339e-11eb-8505-6ff5479b62df","5fc5e5264f08904fd7c87f69").
teaches("91e65b22-339e-11eb-8505-6ff5479b62df","5fc5e52dd58aa03875b9fb2b").
teaches("91e6820a-339e-11eb-b0d8-bfe9168350bd","5fc5e534b7035630e8ec8292").
teaches("91e63444-339e-11eb-8354-e7cb5b5b888e","5fc5e53bfbeb0baecec700ff").
teaches("91e63444-339e-11eb-8354-e7cb5b5b888e","5fc5e5422b026960995bb91b").
teaches("91e6820a-339e-11eb-b0d8-bfe9168350bd","5fc5e5489005cc3067ccb44e").
teaches("91e6820a-339e-11eb-9470-1b2eae064281","5fc5e550bbc093c82678bb8a").
teaches("91e6a910-339e-11eb-a502-5f4651b13629","5fc5e56034906550ebbf4e73").
teaches("91e6820a-339e-11eb-b0d8-bfe9168350bd","5fc5e565b00c34cef303a8d3").
teaches("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4","5fc5e56ce9b206f06ab7dcc7").
teaches("91e6cee0-339e-11eb-8791-7b2ecfcb57da","5fc5e5724dbd5a8a2b1d8676").
teaches("91e6cee0-339e-11eb-8791-7b2ecfcb57da","5fc5e578437343e26e49fd00").
teaches("91e54a7a-339e-11eb-b04b-9f53fc5784a4","5fc5e57f8ad2cfd4d48ba876").
teaches("91e6f6ea-339e-11eb-b667-3fd4d063c2ee","5fc5e585c940f00a5c7631e2").
teaches("91e4f318-339e-11eb-9da0-1bf1c4c0e4d4","5fc5e58acdfef25a6d495a93").
teaches("91e54a7a-339e-11eb-8f3c-bf626a7840c3","5fc5e59329756063c0560816").

% Student-class correspondence table
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e4838d0bcf210dbe3774").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e4838d0bcf210dbe3774").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e4838d0bcf210dbe3774").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e4838d0bcf210dbe3774").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e4838d0bcf210dbe3774").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e4838d0bcf210dbe3774").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e4838d0bcf210dbe3774").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e4838d0bcf210dbe3774").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e4838d0bcf210dbe3774").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e4838d0bcf210dbe3774").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e4838d0bcf210dbe3774").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e4838d0bcf210dbe3774").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e4838d0bcf210dbe3774").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e4838d0bcf210dbe3774").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e4838d0bcf210dbe3774").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e4838d0bcf210dbe3774").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e4838d0bcf210dbe3774").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e4838d0bcf210dbe3774").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e4838d0bcf210dbe3774").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e4838d0bcf210dbe3774").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e4838d0bcf210dbe3774").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e4838d0bcf210dbe3774").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e4838d0bcf210dbe3774").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e4838d0bcf210dbe3774").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e4838d0bcf210dbe3774").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e4838d0bcf210dbe3774").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e4838d0bcf210dbe3774").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e4838d0bcf210dbe3774").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e4838d0bcf210dbe3774").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e4838d0bcf210dbe3774").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e4838d0bcf210dbe3774").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e4838d0bcf210dbe3774").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e4838d0bcf210dbe3774").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e4838d0bcf210dbe3774").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e4838d0bcf210dbe3774").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e4838d0bcf210dbe3774").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e4838d0bcf210dbe3774").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e4838d0bcf210dbe3774").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e4838d0bcf210dbe3774").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e4838d0bcf210dbe3774").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e4838d0bcf210dbe3774").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e4838d0bcf210dbe3774").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e4838d0bcf210dbe3774").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e4838d0bcf210dbe3774").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e4838d0bcf210dbe3774").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e4838d0bcf210dbe3774").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e49ced5eeead04eeb2f9").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e49ced5eeead04eeb2f9").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e49ced5eeead04eeb2f9").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e49ced5eeead04eeb2f9").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e49ced5eeead04eeb2f9").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e49ced5eeead04eeb2f9").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e49ced5eeead04eeb2f9").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e49ced5eeead04eeb2f9").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e49ced5eeead04eeb2f9").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e49ced5eeead04eeb2f9").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e49ced5eeead04eeb2f9").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e49ced5eeead04eeb2f9").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e49ced5eeead04eeb2f9").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e49ced5eeead04eeb2f9").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e49ced5eeead04eeb2f9").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e49ced5eeead04eeb2f9").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e49ced5eeead04eeb2f9").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e49ced5eeead04eeb2f9").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e49ced5eeead04eeb2f9").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e49ced5eeead04eeb2f9").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e49ced5eeead04eeb2f9").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e49ced5eeead04eeb2f9").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e49ced5eeead04eeb2f9").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e49ced5eeead04eeb2f9").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e49ced5eeead04eeb2f9").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e4a5b4e8896a9991064d").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e4a5b4e8896a9991064d").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e4a5b4e8896a9991064d").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e4a5b4e8896a9991064d").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e4a5b4e8896a9991064d").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e4a5b4e8896a9991064d").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e4a5b4e8896a9991064d").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e4a5b4e8896a9991064d").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e4a5b4e8896a9991064d").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e4a5b4e8896a9991064d").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e4a5b4e8896a9991064d").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e4a5b4e8896a9991064d").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e4a5b4e8896a9991064d").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e4a5b4e8896a9991064d").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e4a5b4e8896a9991064d").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e4a5b4e8896a9991064d").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e4a5b4e8896a9991064d").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e4a5b4e8896a9991064d").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e4a5b4e8896a9991064d").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e4a5b4e8896a9991064d").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e4a5b4e8896a9991064d").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e4a5b4e8896a9991064d").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e4a5b4e8896a9991064d").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e4a5b4e8896a9991064d").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e4a5b4e8896a9991064d").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e4a5b4e8896a9991064d").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e4a5b4e8896a9991064d").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e4a5b4e8896a9991064d").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e4a5b4e8896a9991064d").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e4ad0fd3a0c96e51bb6a").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e4ad0fd3a0c96e51bb6a").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e4ad0fd3a0c96e51bb6a").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e4ad0fd3a0c96e51bb6a").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e4ad0fd3a0c96e51bb6a").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e4ad0fd3a0c96e51bb6a").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e4ad0fd3a0c96e51bb6a").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e4ad0fd3a0c96e51bb6a").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e4ad0fd3a0c96e51bb6a").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e4ad0fd3a0c96e51bb6a").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e4ad0fd3a0c96e51bb6a").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e4ad0fd3a0c96e51bb6a").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e4ad0fd3a0c96e51bb6a").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e4ad0fd3a0c96e51bb6a").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e4ad0fd3a0c96e51bb6a").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e4ad0fd3a0c96e51bb6a").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e4ad0fd3a0c96e51bb6a").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e4ad0fd3a0c96e51bb6a").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e4ad0fd3a0c96e51bb6a").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e4ad0fd3a0c96e51bb6a").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e4ad0fd3a0c96e51bb6a").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e4ad0fd3a0c96e51bb6a").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e4ad0fd3a0c96e51bb6a").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e4ad0fd3a0c96e51bb6a").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e4b5415574b82c0d8115").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e4b5415574b82c0d8115").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e4b5415574b82c0d8115").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e4b5415574b82c0d8115").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e4b5415574b82c0d8115").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e4b5415574b82c0d8115").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e4b5415574b82c0d8115").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e4b5415574b82c0d8115").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e4b5415574b82c0d8115").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e4b5415574b82c0d8115").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e4b5415574b82c0d8115").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e4b5415574b82c0d8115").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e4b5415574b82c0d8115").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e4b5415574b82c0d8115").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e4b5415574b82c0d8115").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e4b5415574b82c0d8115").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e4b5415574b82c0d8115").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e4b5415574b82c0d8115").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e4b5415574b82c0d8115").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e4b5415574b82c0d8115").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e4b5415574b82c0d8115").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e4b5415574b82c0d8115").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e4b5415574b82c0d8115").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e4b5415574b82c0d8115").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e4b5415574b82c0d8115").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e4b5415574b82c0d8115").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e4b5415574b82c0d8115").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e4b5415574b82c0d8115").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e4b5415574b82c0d8115").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e4b5415574b82c0d8115").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e4b5415574b82c0d8115").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e4b5415574b82c0d8115").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e4b5415574b82c0d8115").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e4b5415574b82c0d8115").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e4b5415574b82c0d8115").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e4b5415574b82c0d8115").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e4b5415574b82c0d8115").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e4b5415574b82c0d8115").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e4b5415574b82c0d8115").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e4c00b15ca7050a8b045").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e4c00b15ca7050a8b045").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e4c00b15ca7050a8b045").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e4c00b15ca7050a8b045").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e4c00b15ca7050a8b045").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e4c00b15ca7050a8b045").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e4c00b15ca7050a8b045").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e4c00b15ca7050a8b045").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e4c00b15ca7050a8b045").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e4c00b15ca7050a8b045").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e4c00b15ca7050a8b045").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e4c00b15ca7050a8b045").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e4c00b15ca7050a8b045").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e4c00b15ca7050a8b045").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e4c00b15ca7050a8b045").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e4c00b15ca7050a8b045").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e4c00b15ca7050a8b045").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e4c00b15ca7050a8b045").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e4c00b15ca7050a8b045").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e4c00b15ca7050a8b045").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e4c00b15ca7050a8b045").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e4c00b15ca7050a8b045").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e4c8af1d16f6b72abe0c").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e4c8af1d16f6b72abe0c").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e4c8af1d16f6b72abe0c").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e4c8af1d16f6b72abe0c").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e4c8af1d16f6b72abe0c").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e4c8af1d16f6b72abe0c").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e4c8af1d16f6b72abe0c").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e4c8af1d16f6b72abe0c").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e4c8af1d16f6b72abe0c").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e4c8af1d16f6b72abe0c").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e4c8af1d16f6b72abe0c").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e4c8af1d16f6b72abe0c").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e4c8af1d16f6b72abe0c").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e4c8af1d16f6b72abe0c").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e4c8af1d16f6b72abe0c").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e4d5a08e6abc491ec5da").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e4d5a08e6abc491ec5da").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e4d5a08e6abc491ec5da").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e4d5a08e6abc491ec5da").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e4d5a08e6abc491ec5da").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e4d5a08e6abc491ec5da").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e4d5a08e6abc491ec5da").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e4d5a08e6abc491ec5da").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e4d5a08e6abc491ec5da").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e4d5a08e6abc491ec5da").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e4d5a08e6abc491ec5da").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e4d5a08e6abc491ec5da").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e4d5a08e6abc491ec5da").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e4d5a08e6abc491ec5da").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e4d5a08e6abc491ec5da").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e4d5a08e6abc491ec5da").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e4d5a08e6abc491ec5da").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e4d5a08e6abc491ec5da").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e4d5a08e6abc491ec5da").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e4d5a08e6abc491ec5da").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e4d5a08e6abc491ec5da").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e4d5a08e6abc491ec5da").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e4d5a08e6abc491ec5da").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e4d5a08e6abc491ec5da").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e4d5a08e6abc491ec5da").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e4d5a08e6abc491ec5da").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e4d5a08e6abc491ec5da").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e4d5a08e6abc491ec5da").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e4d5a08e6abc491ec5da").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e4d5a08e6abc491ec5da").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e4d5a08e6abc491ec5da").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e4d5a08e6abc491ec5da").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e4d5a08e6abc491ec5da").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e4d5a08e6abc491ec5da").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e4d5a08e6abc491ec5da").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e4d5a08e6abc491ec5da").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e4d5a08e6abc491ec5da").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e4d5a08e6abc491ec5da").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e4d5a08e6abc491ec5da").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e4d5a08e6abc491ec5da").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e4d5a08e6abc491ec5da").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e4d5a08e6abc491ec5da").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e4d5a08e6abc491ec5da").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e4d5a08e6abc491ec5da").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e4d5a08e6abc491ec5da").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e4d5a08e6abc491ec5da").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e4dcc902e9650b60228b").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e4dcc902e9650b60228b").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e4dcc902e9650b60228b").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e4dcc902e9650b60228b").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e4dcc902e9650b60228b").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e4dcc902e9650b60228b").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e4dcc902e9650b60228b").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e4dcc902e9650b60228b").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e4dcc902e9650b60228b").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e4dcc902e9650b60228b").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e4dcc902e9650b60228b").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e4dcc902e9650b60228b").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e4dcc902e9650b60228b").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e4dcc902e9650b60228b").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e4dcc902e9650b60228b").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e4dcc902e9650b60228b").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e4dcc902e9650b60228b").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e4dcc902e9650b60228b").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e4dcc902e9650b60228b").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e4dcc902e9650b60228b").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e4dcc902e9650b60228b").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e4dcc902e9650b60228b").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e4e43557c3e4e8a47bff").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e4e43557c3e4e8a47bff").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e4e43557c3e4e8a47bff").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e4e43557c3e4e8a47bff").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e4e43557c3e4e8a47bff").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e4e43557c3e4e8a47bff").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e4e43557c3e4e8a47bff").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e4e43557c3e4e8a47bff").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e4e43557c3e4e8a47bff").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e4e43557c3e4e8a47bff").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e4e43557c3e4e8a47bff").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e4e43557c3e4e8a47bff").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e4e43557c3e4e8a47bff").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e4e43557c3e4e8a47bff").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e4e43557c3e4e8a47bff").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e4e43557c3e4e8a47bff").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e4e43557c3e4e8a47bff").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e4e43557c3e4e8a47bff").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e4e43557c3e4e8a47bff").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e4e43557c3e4e8a47bff").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e4e43557c3e4e8a47bff").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e4e43557c3e4e8a47bff").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e4e43557c3e4e8a47bff").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e4e43557c3e4e8a47bff").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e4e43557c3e4e8a47bff").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e4e43557c3e4e8a47bff").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e4e43557c3e4e8a47bff").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e4e43557c3e4e8a47bff").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e4e43557c3e4e8a47bff").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e4e43557c3e4e8a47bff").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e4e43557c3e4e8a47bff").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e4e43557c3e4e8a47bff").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e4e43557c3e4e8a47bff").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e4e43557c3e4e8a47bff").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e4e43557c3e4e8a47bff").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e4e43557c3e4e8a47bff").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e4e43557c3e4e8a47bff").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e4e43557c3e4e8a47bff").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e4e43557c3e4e8a47bff").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e4e43557c3e4e8a47bff").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e4e43557c3e4e8a47bff").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e4e43557c3e4e8a47bff").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e4e43557c3e4e8a47bff").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e4e43557c3e4e8a47bff").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e4e43557c3e4e8a47bff").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e4e43557c3e4e8a47bff").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e4ed79b5505f5963e147").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e4ed79b5505f5963e147").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e4ed79b5505f5963e147").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e4ed79b5505f5963e147").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e4ed79b5505f5963e147").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e4ed79b5505f5963e147").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e4ed79b5505f5963e147").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e4ed79b5505f5963e147").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e4ed79b5505f5963e147").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e4ed79b5505f5963e147").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e4ed79b5505f5963e147").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e4ed79b5505f5963e147").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e4ed79b5505f5963e147").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e4ed79b5505f5963e147").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e4f41343a497cffa6f53").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e4f41343a497cffa6f53").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e4f41343a497cffa6f53").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e4f41343a497cffa6f53").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e4f41343a497cffa6f53").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e4f41343a497cffa6f53").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e4f41343a497cffa6f53").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e4f41343a497cffa6f53").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e4f41343a497cffa6f53").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e4f41343a497cffa6f53").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e4f41343a497cffa6f53").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e4f41343a497cffa6f53").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e4f41343a497cffa6f53").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e4f41343a497cffa6f53").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e4fbb89c74d6e93b1c29").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e4fbb89c74d6e93b1c29").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e4fbb89c74d6e93b1c29").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e4fbb89c74d6e93b1c29").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e4fbb89c74d6e93b1c29").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e4fbb89c74d6e93b1c29").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e4fbb89c74d6e93b1c29").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e4fbb89c74d6e93b1c29").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e4fbb89c74d6e93b1c29").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e4fbb89c74d6e93b1c29").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e4fbb89c74d6e93b1c29").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e4fbb89c74d6e93b1c29").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e4fbb89c74d6e93b1c29").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e4fbb89c74d6e93b1c29").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e4fbb89c74d6e93b1c29").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e4fbb89c74d6e93b1c29").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e4fbb89c74d6e93b1c29").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e4fbb89c74d6e93b1c29").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e4fbb89c74d6e93b1c29").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e4fbb89c74d6e93b1c29").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e4fbb89c74d6e93b1c29").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e4fbb89c74d6e93b1c29").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e4fbb89c74d6e93b1c29").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e4fbb89c74d6e93b1c29").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e4fbb89c74d6e93b1c29").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e4fbb89c74d6e93b1c29").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e4fbb89c74d6e93b1c29").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e4fbb89c74d6e93b1c29").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e4fbb89c74d6e93b1c29").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e4fbb89c74d6e93b1c29").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e4fbb89c74d6e93b1c29").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e4fbb89c74d6e93b1c29").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e4fbb89c74d6e93b1c29").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e4fbb89c74d6e93b1c29").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e4fbb89c74d6e93b1c29").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e4fbb89c74d6e93b1c29").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e4fbb89c74d6e93b1c29").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e4fbb89c74d6e93b1c29").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e4fbb89c74d6e93b1c29").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e4fbb89c74d6e93b1c29").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e4fbb89c74d6e93b1c29").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e4fbb89c74d6e93b1c29").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e4fbb89c74d6e93b1c29").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e4fbb89c74d6e93b1c29").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e4fbb89c74d6e93b1c29").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e4fbb89c74d6e93b1c29").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e50303a0d86ae52e066f").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e50303a0d86ae52e066f").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e50303a0d86ae52e066f").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e50303a0d86ae52e066f").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e50303a0d86ae52e066f").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e50303a0d86ae52e066f").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e50303a0d86ae52e066f").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e50303a0d86ae52e066f").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e5095c42bb58a7e4b503").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e5095c42bb58a7e4b503").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e5095c42bb58a7e4b503").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e5095c42bb58a7e4b503").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e5095c42bb58a7e4b503").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e5095c42bb58a7e4b503").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e5095c42bb58a7e4b503").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e5095c42bb58a7e4b503").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e5112886ed2aa8037c68").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e5112886ed2aa8037c68").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e5112886ed2aa8037c68").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e5112886ed2aa8037c68").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e5112886ed2aa8037c68").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e5112886ed2aa8037c68").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e5112886ed2aa8037c68").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e5112886ed2aa8037c68").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e51858d6e87fd58449be").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e51858d6e87fd58449be").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e51858d6e87fd58449be").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e51858d6e87fd58449be").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e51858d6e87fd58449be").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e51858d6e87fd58449be").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e51858d6e87fd58449be").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e51858d6e87fd58449be").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e51fd3f3fb935504ade5").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e51fd3f3fb935504ade5").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e51fd3f3fb935504ade5").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e51fd3f3fb935504ade5").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e51fd3f3fb935504ade5").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e51fd3f3fb935504ade5").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e51fd3f3fb935504ade5").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e51fd3f3fb935504ade5").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e51fd3f3fb935504ade5").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e51fd3f3fb935504ade5").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e51fd3f3fb935504ade5").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e51fd3f3fb935504ade5").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e51fd3f3fb935504ade5").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e51fd3f3fb935504ade5").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e51fd3f3fb935504ade5").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e51fd3f3fb935504ade5").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e51fd3f3fb935504ade5").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e51fd3f3fb935504ade5").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e51fd3f3fb935504ade5").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e51fd3f3fb935504ade5").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e51fd3f3fb935504ade5").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e51fd3f3fb935504ade5").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e51fd3f3fb935504ade5").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e51fd3f3fb935504ade5").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e51fd3f3fb935504ade5").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e51fd3f3fb935504ade5").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e51fd3f3fb935504ade5").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e51fd3f3fb935504ade5").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e51fd3f3fb935504ade5").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e51fd3f3fb935504ade5").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e51fd3f3fb935504ade5").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e51fd3f3fb935504ade5").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e51fd3f3fb935504ade5").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e51fd3f3fb935504ade5").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e51fd3f3fb935504ade5").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e51fd3f3fb935504ade5").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e51fd3f3fb935504ade5").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e51fd3f3fb935504ade5").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e51fd3f3fb935504ade5").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e51fd3f3fb935504ade5").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e51fd3f3fb935504ade5").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e51fd3f3fb935504ade5").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e51fd3f3fb935504ade5").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e51fd3f3fb935504ade5").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e51fd3f3fb935504ade5").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e51fd3f3fb935504ade5").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e5264f08904fd7c87f69").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e5264f08904fd7c87f69").
follows("6419900e-339b-11eb-9a1d-c315bd6d2984","5fc5e5264f08904fd7c87f69").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e5264f08904fd7c87f69").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e5264f08904fd7c87f69").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e5264f08904fd7c87f69").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e5264f08904fd7c87f69").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e5264f08904fd7c87f69").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e5264f08904fd7c87f69").
follows("641bff56-339b-11eb-ab5f-47b7a6886f0b","5fc5e5264f08904fd7c87f69").
follows("641c2652-339b-11eb-a62a-afa8247997cd","5fc5e5264f08904fd7c87f69").
follows("641c7440-339b-11eb-9e2f-d7c4172df0b2","5fc5e5264f08904fd7c87f69").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e5264f08904fd7c87f69").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e5264f08904fd7c87f69").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e5264f08904fd7c87f69").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e5264f08904fd7c87f69").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e5264f08904fd7c87f69").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e5264f08904fd7c87f69").
follows("641dd420-339b-11eb-9c40-abd2d1f0e135","5fc5e5264f08904fd7c87f69").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e5264f08904fd7c87f69").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e5264f08904fd7c87f69").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e5264f08904fd7c87f69").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e5264f08904fd7c87f69").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e5264f08904fd7c87f69").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e5264f08904fd7c87f69").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e5264f08904fd7c87f69").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e5264f08904fd7c87f69").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e5264f08904fd7c87f69").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e5264f08904fd7c87f69").
follows("642092a0-339b-11eb-a19b-f31171bb6e48","5fc5e5264f08904fd7c87f69").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e5264f08904fd7c87f69").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e5264f08904fd7c87f69").
follows("6421a474-339b-11eb-8414-87ec1119a40f","5fc5e5264f08904fd7c87f69").
follows("6421a474-339b-11eb-9732-3307eb5ccaa9","5fc5e5264f08904fd7c87f69").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e5264f08904fd7c87f69").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e5264f08904fd7c87f69").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e5264f08904fd7c87f69").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e52dd58aa03875b9fb2b").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e52dd58aa03875b9fb2b").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e52dd58aa03875b9fb2b").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e52dd58aa03875b9fb2b").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e52dd58aa03875b9fb2b").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e52dd58aa03875b9fb2b").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e52dd58aa03875b9fb2b").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e52dd58aa03875b9fb2b").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e52dd58aa03875b9fb2b").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e52dd58aa03875b9fb2b").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e52dd58aa03875b9fb2b").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e52dd58aa03875b9fb2b").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e52dd58aa03875b9fb2b").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e52dd58aa03875b9fb2b").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e52dd58aa03875b9fb2b").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e52dd58aa03875b9fb2b").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e52dd58aa03875b9fb2b").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e534b7035630e8ec8292").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e534b7035630e8ec8292").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e534b7035630e8ec8292").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e534b7035630e8ec8292").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e534b7035630e8ec8292").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e534b7035630e8ec8292").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e534b7035630e8ec8292").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e534b7035630e8ec8292").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e534b7035630e8ec8292").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e534b7035630e8ec8292").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e534b7035630e8ec8292").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e534b7035630e8ec8292").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e534b7035630e8ec8292").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e534b7035630e8ec8292").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e534b7035630e8ec8292").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e534b7035630e8ec8292").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e534b7035630e8ec8292").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e53bfbeb0baecec700ff").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e53bfbeb0baecec700ff").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e53bfbeb0baecec700ff").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e53bfbeb0baecec700ff").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e53bfbeb0baecec700ff").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e53bfbeb0baecec700ff").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e53bfbeb0baecec700ff").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e53bfbeb0baecec700ff").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e53bfbeb0baecec700ff").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e53bfbeb0baecec700ff").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e53bfbeb0baecec700ff").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e53bfbeb0baecec700ff").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e53bfbeb0baecec700ff").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e53bfbeb0baecec700ff").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e53bfbeb0baecec700ff").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e53bfbeb0baecec700ff").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e53bfbeb0baecec700ff").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e5422b026960995bb91b").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e5422b026960995bb91b").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e5422b026960995bb91b").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e5422b026960995bb91b").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e5422b026960995bb91b").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e5422b026960995bb91b").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e5422b026960995bb91b").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e5422b026960995bb91b").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e5422b026960995bb91b").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e5422b026960995bb91b").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e5422b026960995bb91b").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e5422b026960995bb91b").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e5422b026960995bb91b").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e5422b026960995bb91b").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e5422b026960995bb91b").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e5422b026960995bb91b").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e5422b026960995bb91b").
follows("641b638e-339b-11eb-9995-d377ee365170","5fc5e5489005cc3067ccb44e").
follows("641cc10c-339b-11eb-a741-c7ea85a5f4fd","5fc5e5489005cc3067ccb44e").
follows("641dfb30-339b-11eb-9994-d39811fb5f49","5fc5e5489005cc3067ccb44e").
follows("641e4950-339b-11eb-aef1-8b13b7ac7249","5fc5e5489005cc3067ccb44e").
follows("641e6ff2-339b-11eb-aeb5-cbf5b6e708c5","5fc5e5489005cc3067ccb44e").
follows("64201cb2-339b-11eb-a79f-6fc93b31deb8","5fc5e5489005cc3067ccb44e").
follows("6418cc14-339b-11eb-96d9-3fa2ca4a45d4","5fc5e550bbc093c82678bb8a").
follows("6418f31a-339b-11eb-8c5d-0bd22fc07c59","5fc5e550bbc093c82678bb8a").
follows("641fcd7a-339b-11eb-bdc5-139e1799cfa8","5fc5e550bbc093c82678bb8a").
follows("64204372-339b-11eb-88d5-f359a213717b","5fc5e550bbc093c82678bb8a").
follows("6422dd58-339b-11eb-b3fa-e3ee10890d13","5fc5e550bbc093c82678bb8a").
follows("641bb172-339b-11eb-a56c-f73126eed4a0","5fc5e56034906550ebbf4e73").
follows("641b3c92-339b-11eb-9ac9-bff7de771a78","5fc5e565b00c34cef303a8d3").
follows("641c4d4e-339b-11eb-bd92-6f7b011dc689","5fc5e565b00c34cef303a8d3").
follows("641ce7fe-339b-11eb-9bc4-572b5e786fbc","5fc5e565b00c34cef303a8d3").
follows("641ebde0-339b-11eb-b58b-a7e66a0d591c","5fc5e565b00c34cef303a8d3").
follows("6421ca4e-339b-11eb-9857-3f0cbc8ef02e","5fc5e565b00c34cef303a8d3").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e56ce9b206f06ab7dcc7").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e56ce9b206f06ab7dcc7").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e56ce9b206f06ab7dcc7").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e56ce9b206f06ab7dcc7").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e56ce9b206f06ab7dcc7").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e56ce9b206f06ab7dcc7").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e56ce9b206f06ab7dcc7").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e56ce9b206f06ab7dcc7").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e56ce9b206f06ab7dcc7").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e56ce9b206f06ab7dcc7").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e56ce9b206f06ab7dcc7").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e56ce9b206f06ab7dcc7").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e56ce9b206f06ab7dcc7").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e56ce9b206f06ab7dcc7").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e56ce9b206f06ab7dcc7").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e56ce9b206f06ab7dcc7").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e56ce9b206f06ab7dcc7").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e56ce9b206f06ab7dcc7").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e56ce9b206f06ab7dcc7").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e56ce9b206f06ab7dcc7").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e56ce9b206f06ab7dcc7").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e56ce9b206f06ab7dcc7").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e56ce9b206f06ab7dcc7").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e56ce9b206f06ab7dcc7").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e56ce9b206f06ab7dcc7").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e56ce9b206f06ab7dcc7").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e56ce9b206f06ab7dcc7").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e56ce9b206f06ab7dcc7").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e56ce9b206f06ab7dcc7").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e56ce9b206f06ab7dcc7").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e56ce9b206f06ab7dcc7").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e56ce9b206f06ab7dcc7").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e56ce9b206f06ab7dcc7").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e56ce9b206f06ab7dcc7").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e56ce9b206f06ab7dcc7").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e56ce9b206f06ab7dcc7").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e56ce9b206f06ab7dcc7").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e56ce9b206f06ab7dcc7").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e56ce9b206f06ab7dcc7").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e56ce9b206f06ab7dcc7").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e56ce9b206f06ab7dcc7").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e56ce9b206f06ab7dcc7").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e56ce9b206f06ab7dcc7").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e56ce9b206f06ab7dcc7").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e56ce9b206f06ab7dcc7").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e56ce9b206f06ab7dcc7").
follows("64187cf0-339b-11eb-83ee-578bccab851e","5fc5e5724dbd5a8a2b1d8676").
follows("6418a3ec-339b-11eb-84fa-3ff9d6404d5f","5fc5e5724dbd5a8a2b1d8676").
follows("6418cc14-339b-11eb-a8e7-0f7974624d87","5fc5e5724dbd5a8a2b1d8676").
follows("64191b24-339b-11eb-8574-274d853701cc","5fc5e5724dbd5a8a2b1d8676").
follows("64196912-339b-11eb-9bab-8f8eb7ec2154","5fc5e5724dbd5a8a2b1d8676").
follows("64196912-339b-11eb-814c-9f6edcae850b","5fc5e5724dbd5a8a2b1d8676").
follows("6419b700-339b-11eb-9f52-efcef6eaaab9","5fc5e5724dbd5a8a2b1d8676").
follows("641a053e-339b-11eb-be81-8335d8c0acc9","5fc5e5724dbd5a8a2b1d8676").
follows("641a053e-339b-11eb-b744-4bf524ef3244","5fc5e5724dbd5a8a2b1d8676").
follows("641a2abe-339b-11eb-8c1c-efce54bb2f93","5fc5e5724dbd5a8a2b1d8676").
follows("641a52dc-339b-11eb-89fc-17df5c86ebab","5fc5e5724dbd5a8a2b1d8676").
follows("641a79ce-339b-11eb-9ea4-3b33e1eca2b3","5fc5e5724dbd5a8a2b1d8676").
follows("641aa0c0-339b-11eb-9b48-131888dee1cf","5fc5e5724dbd5a8a2b1d8676").
follows("641ac7bc-339b-11eb-9c68-ff323341e980","5fc5e5724dbd5a8a2b1d8676").
follows("641aeeae-339b-11eb-8802-079e39f1f05f","5fc5e5724dbd5a8a2b1d8676").
follows("641b1582-339b-11eb-b6b0-5323f4c03010","5fc5e5724dbd5a8a2b1d8676").
follows("641b3c92-339b-11eb-a059-73e8b65d448c","5fc5e5724dbd5a8a2b1d8676").
follows("641b8a80-339b-11eb-9176-0bf9c2c6f510","5fc5e5724dbd5a8a2b1d8676").
follows("641b8a80-339b-11eb-ad54-8bcbdfdaefc4","5fc5e5724dbd5a8a2b1d8676").
follows("641bd882-339b-11eb-941d-230e45752110","5fc5e5724dbd5a8a2b1d8676").
follows("641c2652-339b-11eb-9d2d-8368da738a8e","5fc5e5724dbd5a8a2b1d8676").
follows("641d370e-339b-11eb-bf6a-577a9d353e42","5fc5e5724dbd5a8a2b1d8676").
follows("641d370e-339b-11eb-a829-37809ea78594","5fc5e5724dbd5a8a2b1d8676").
follows("641d8646-339b-11eb-be4c-df1ee7a7f071","5fc5e5724dbd5a8a2b1d8676").
follows("641dad2e-339b-11eb-8acb-e3c82f521e7a","5fc5e5724dbd5a8a2b1d8676").
follows("641e220e-339b-11eb-a178-43e209eb16e9","5fc5e5724dbd5a8a2b1d8676").
follows("641e220e-339b-11eb-a226-6310cc33a3e5","5fc5e5724dbd5a8a2b1d8676").
follows("641e6ff2-339b-11eb-bd87-17f98c15ee73","5fc5e5724dbd5a8a2b1d8676").
follows("641f0c46-339b-11eb-a1b8-0fed0cbb4f01","5fc5e5724dbd5a8a2b1d8676").
follows("641f59c6-339b-11eb-8ca1-cf9bdd97cc01","5fc5e5724dbd5a8a2b1d8676").
follows("641f7f8c-339b-11eb-a510-0f88f15e37ce","5fc5e5724dbd5a8a2b1d8676").
follows("641f7f8c-339b-11eb-828a-cf225f289f3c","5fc5e5724dbd5a8a2b1d8676").
follows("64206ba4-339b-11eb-b822-c3750880c936","5fc5e5724dbd5a8a2b1d8676").
follows("642092a0-339b-11eb-8ddd-6fd8c8cd673f","5fc5e5724dbd5a8a2b1d8676").
follows("6420b99c-339b-11eb-8015-833111372de0","5fc5e5724dbd5a8a2b1d8676").
follows("6420e1b0-339b-11eb-afba-2bf409d54f4e","5fc5e5724dbd5a8a2b1d8676").
follows("64210898-339b-11eb-8a56-c7c6e18741da","5fc5e5724dbd5a8a2b1d8676").
follows("64212fe4-339b-11eb-976f-0b2d79b3ac50","5fc5e5724dbd5a8a2b1d8676").
follows("64215686-339b-11eb-93fc-a33d34907ac6","5fc5e5724dbd5a8a2b1d8676").
follows("64215686-339b-11eb-b851-db350caa1621","5fc5e5724dbd5a8a2b1d8676").
follows("6421f262-339b-11eb-bd4c-7bd3cc0b0a76","5fc5e5724dbd5a8a2b1d8676").
follows("64221846-339b-11eb-8a6d-4f73323a093e","5fc5e5724dbd5a8a2b1d8676").
follows("64224172-339b-11eb-b675-67a0f9b5992b","5fc5e5724dbd5a8a2b1d8676").
follows("64226878-339b-11eb-b444-835920aaaaff","5fc5e5724dbd5a8a2b1d8676").
follows("64228f6a-339b-11eb-94a9-e37ea60c3823","5fc5e5724dbd5a8a2b1d8676").
follows("6422b666-339b-11eb-941c-9363325fb5cb","5fc5e5724dbd5a8a2b1d8676").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e578437343e26e49fd00").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e578437343e26e49fd00").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e578437343e26e49fd00").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e578437343e26e49fd00").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e578437343e26e49fd00").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e578437343e26e49fd00").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e578437343e26e49fd00").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e578437343e26e49fd00").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e578437343e26e49fd00").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e578437343e26e49fd00").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e578437343e26e49fd00").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e578437343e26e49fd00").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e578437343e26e49fd00").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e578437343e26e49fd00").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e578437343e26e49fd00").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e57f8ad2cfd4d48ba876").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e57f8ad2cfd4d48ba876").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e57f8ad2cfd4d48ba876").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e57f8ad2cfd4d48ba876").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e57f8ad2cfd4d48ba876").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e57f8ad2cfd4d48ba876").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e57f8ad2cfd4d48ba876").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e57f8ad2cfd4d48ba876").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e57f8ad2cfd4d48ba876").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e57f8ad2cfd4d48ba876").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e57f8ad2cfd4d48ba876").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e57f8ad2cfd4d48ba876").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e57f8ad2cfd4d48ba876").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e57f8ad2cfd4d48ba876").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e57f8ad2cfd4d48ba876").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e585c940f00a5c7631e2").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e585c940f00a5c7631e2").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e585c940f00a5c7631e2").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e585c940f00a5c7631e2").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e585c940f00a5c7631e2").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e585c940f00a5c7631e2").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e585c940f00a5c7631e2").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e585c940f00a5c7631e2").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e585c940f00a5c7631e2").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e585c940f00a5c7631e2").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e585c940f00a5c7631e2").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e585c940f00a5c7631e2").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e585c940f00a5c7631e2").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e585c940f00a5c7631e2").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e585c940f00a5c7631e2").
follows("64191b24-339b-11eb-a30f-eb57f891bc68","5fc5e58acdfef25a6d495a93").
follows("6419b700-339b-11eb-92fd-3722c3919b00","5fc5e58acdfef25a6d495a93").
follows("6419ddde-339b-11eb-a9d4-fff5cbb496b0","5fc5e58acdfef25a6d495a93").
follows("641a52dc-339b-11eb-9fb1-0392c000ed25","5fc5e58acdfef25a6d495a93").
follows("641bd882-339b-11eb-ab0e-375dddd4fb29","5fc5e58acdfef25a6d495a93").
follows("641c9a1a-339b-11eb-a1ee-8f889806076d","5fc5e58acdfef25a6d495a93").
follows("641c9a1a-339b-11eb-97d8-3fe8fbdb978d","5fc5e58acdfef25a6d495a93").
follows("641d5e1e-339b-11eb-b613-53b5d7feea1a","5fc5e58acdfef25a6d495a93").
follows("641d8646-339b-11eb-9c8f-37624926dd97","5fc5e58acdfef25a6d495a93").
follows("641dd420-339b-11eb-bf1f-c755c01eb5ba","5fc5e58acdfef25a6d495a93").
follows("641ebde0-339b-11eb-9887-a752c9637f99","5fc5e58acdfef25a6d495a93").
follows("641f32d4-339b-11eb-b3b2-a3a73bc44170","5fc5e58acdfef25a6d495a93").
follows("641fcd7a-339b-11eb-97a9-87c213b41bd5","5fc5e58acdfef25a6d495a93").
follows("64221846-339b-11eb-8073-9f7280b9039a","5fc5e58acdfef25a6d495a93").
follows("6422b666-339b-11eb-900b-c76c27083730","5fc5e58acdfef25a6d495a93").
follows("64194220-339b-11eb-8e1a-bf1891ab09b4","5fc5e59329756063c0560816").
follows("641ac7bc-339b-11eb-9f5b-3340438c7a61","5fc5e59329756063c0560816").
follows("641aeeae-339b-11eb-902b-0398cbf76a48","5fc5e59329756063c0560816").
follows("641ce7fe-339b-11eb-8851-c32359247cea","5fc5e59329756063c0560816").
follows("641d1030-339b-11eb-a21b-b3b71142377e","5fc5e59329756063c0560816").
follows("641e969e-339b-11eb-ad2f-27a493e23f23","5fc5e59329756063c0560816").
follows("641eea04-339b-11eb-991d-37fc01de0af7","5fc5e59329756063c0560816").
follows("641f32d4-339b-11eb-9cd4-f7bc245d1c70","5fc5e59329756063c0560816").
follows("641fa688-339b-11eb-a09c-c795c334402c","5fc5e59329756063c0560816").
follows("641ff598-339b-11eb-829b-bf4cc9d53b81","5fc5e59329756063c0560816").
follows("64204372-339b-11eb-be6a-8b86e81ee4a2","5fc5e59329756063c0560816").
follows("6420e1b0-339b-11eb-811b-8756ee8ebe55","5fc5e59329756063c0560816").
follows("64217da0-339b-11eb-848d-4f355031aaaf","5fc5e59329756063c0560816").
follows("64226878-339b-11eb-b1d1-6386818a5367","5fc5e59329756063c0560816").
