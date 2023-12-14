:- module(small_data, [
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

%Allow grouping constraints per students/teacher
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
exam_duration(Ex, 2) :- exam(Ex, _).

% Room capacity
classroom_capacity(100, 2). % small room has a capacity of 2
classroom_capacity(200, 4). % large room has a capacity of 4

% Availability of rooms
classroom_available(Room, 1, 10, 12) :- classroom(Room, _). % Day 1, all rooms are available from 10 to 12
classroom_available(Room, 2, 10, 12) :- classroom(Room, _). % Day 2, all rooms are available from 10 to 12
classroom_available(Room, 3, 10, 15) :- classroom(Room, _). % Day 3, all rooms are available from 10 to 15
classroom_available(Room, 4, 10, 12) :- classroom(Room, _). % Day 4, all rooms are available from 10 to 12
classroom_available(Room, 5, 10, 12) :- classroom(Room, _). % Day 5, all rooms are available from 10 to 12

% The exam season runs from the first_day to the last_day
ex_season_starts(1).
ex_season_ends(5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSTRAINTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% How many correction time every exam requires
c_correction_time("5fbb4f2ea40616944a8e6b73", 2).
c_correction_time("5fbb4f7e86c54afb4b70370d", 1).
c_correction_time("5fbb4f89498d05787f0b1e63", 1).
c_correction_time("5fbb4f91248c761433523323", 1).
c_correction_time("5fbb4f9ac118ca4b72c8973f", 2).

% How many study time exams require
c_study_time("5fbb4f2ea40616944a8e6b73", 2).
c_study_time("5fbb4f7e86c54afb4b70370d", 1).
c_study_time("5fbb4f89498d05787f0b1e63", 1).
c_study_time("5fbb4f91248c761433523323", 1).
c_study_time("5fbb4f9ac118ca4b72c8973f", 1).


% Teachers preferences

% All teachers prefer a lunch break
c_lunch_break(Teacher, 1) :- teacher(Teacher, _).

% All teachers prefer not to have exams in a row
c_no_exams_in_row(Teacher, 2) :- teacher(Teacher, _).

% Josef prefers no exams at day 3
c_no_exam_in_period("b108e0cc-2d4c-11eb-adc1-0242ac120002", 3, 0, 24, 5).

% Ann prefers no exams before noon
c_no_exam_in_period("b108e1c6-2d4c-11eb-adc1-0242ac120002", Day, 0, 12, 1) :-
	ex_season_starts(FirstDay),
	ex_season_ends(LastDay),
	between(FirstDay, LastDay, Day).

% John prefers no exams after 14h
c_no_exam_in_period("b108db7c-2d4c-11eb-adc1-0242ac120002", Day, 14, 24, 5) :-
	ex_season_starts(FirstDay),
	ex_season_ends(LastDay),
	between(FirstDay, LastDay, Day).

% Science & technology preferably not day 1
c_not_in_period("b108db7c-2d4c-11eb-adc1-0242ac120002", "5fbb4f7e86c54afb4b70370d", 1, 0, 24, 3).

% All teachers prefer to be guaranteed to have enough correction time
c_not_enough_correction_penalty(Teacher, 3) :- teacher(Teacher, _).


% Students preferences

% All students prefer a lunch break
c_lunch_break(Student, 1) :- student(Student, _).

% All students prefer not to have multiple exams on the same day
c_no_exams_same_day(Student, 2) :- student(Student, _).

% All students prefer not to have exams in a row
c_no_exams_in_row(Student, 5) :- student(Student, _).

% Every student prefers to be guaranteed to have enough study time
c_not_enough_study_penalty(Student, 3) :- student(Student, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% List of classes
%
class("5fbb4f2ea40616944a8e6b73", "Math").
class("5fbb4f7e86c54afb4b70370d", "Science & Technology").
class("5fbb4f89498d05787f0b1e63", "Philosophy").
class("5fbb4f91248c761433523323", "Religion").
class("5fbb4f9ac118ca4b72c8973f", "English").

%
% Exams
%
%class_has_exam("5fbb4f2ea40616944a8e6b73").
%class_has_exam("5fbb4f7e86c54afb4b70370d").
%class_has_exam("5fbb4f89498d05787f0b1e63").
%class_has_exam("5fbb4f91248c761433523323").
%class_has_exam("5fbb4f9ac118ca4b72c8973f").
class_has_exam("5fbb4f7e86c54afb4b70370d").
class_has_exam("5fbb4f89498d05787f0b1e63").
class_has_exam("5fbb4f91248c761433523323").
class_has_exam("5fbb4f9ac118ca4b72c8973f").
class_has_exam("5fbb4f2ea40616944a8e6b73").

%
% Available classrooms
%
classroom(100, "Small room").
classroom(200, "Large room").

%
% People database
%
sp_unit_profile("b108db7c-2d4c-11eb-adc1-0242ac120002", "Mr John", teacher).
sp_unit_profile("b108ddc0-2d4c-11eb-adc1-0242ac120002", "Mr Francis", teacher).
sp_unit_profile("b108e0cc-2d4c-11eb-adc1-0242ac120002", "Mr Josef", teacher).
sp_unit_profile("b108e1c6-2d4c-11eb-adc1-0242ac120002", "Ms Ann", teacher).
sp_unit_profile("b108e2a2-2d4c-11eb-adc1-0242ac120002", "Anna", student).
sp_unit_profile("b108e36a-2d4c-11eb-adc1-0242ac120002", "Max", student).
sp_unit_profile("b108e432-2d4c-11eb-adc1-0242ac120002", "Bill", student).
sp_unit_profile("b108e4fa-2d4c-11eb-adc1-0242ac120002", "Carla", student).

% Teacher-class correspondence table
teaches("b108db7c-2d4c-11eb-adc1-0242ac120002", "5fbb4f2ea40616944a8e6b73").
teaches("b108db7c-2d4c-11eb-adc1-0242ac120002", "5fbb4f7e86c54afb4b70370d").
teaches("b108ddc0-2d4c-11eb-adc1-0242ac120002", "5fbb4f89498d05787f0b1e63").
teaches("b108e0cc-2d4c-11eb-adc1-0242ac120002", "5fbb4f91248c761433523323").
teaches("b108e1c6-2d4c-11eb-adc1-0242ac120002", "5fbb4f9ac118ca4b72c8973f").

% Student-class correspondence table
follows(Student, "5fbb4f2ea40616944a8e6b73") :- student(Student, _). % Every student follows Math
follows(Student, "5fbb4f7e86c54afb4b70370d") :- student(Student, _). % Every student follows Science & Technology
follows(Student, "5fbb4f9ac118ca4b72c8973f") :- student(Student, _). % Every student follows Languages
follows("b108e36a-2d4c-11eb-adc1-0242ac120002", "5fbb4f89498d05787f0b1e63"). % Max follows philosophy
follows("b108e432-2d4c-11eb-adc1-0242ac120002", "5fbb4f89498d05787f0b1e63"). % Bill follows philosophy
follows("b108e2a2-2d4c-11eb-adc1-0242ac120002", "5fbb4f91248c761433523323"). % Anna follows religion
follows("b108e4fa-2d4c-11eb-adc1-0242ac120002", "5fbb4f91248c761433523323"). % Carla follows religion
