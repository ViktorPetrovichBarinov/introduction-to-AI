:- module(cost, [cost/2, schedule_violates_constraint/2]).

:- use_module(large_data).
:- use_module(utils).

% cost(+Schedule, ?Cost)
%		+Schedule: Schedule as a list of events
%		?Cost: Total cost of Schedule
%
% Holds if and only if Cost is a cost of a Schedule.
cost(Schedule, Cost) :- cost(Schedule, Cost, _).

% schedule_violates_constraint(+Scedule, -Violated)
%		+Schedule: Schedule as a list of events
%		-Violated: List of constraints violated by the Schedule
%
% Holds if and only if Violated is a list of soft constraints violated by a Schedule
schedule_violates_constraint(Schedule, Violated) :- cost(Schedule, _, Violated).

% cost(+Schedule, ?Cost, -Violated)
%		+Schedule: Schedule as a list of events
%		?Cost: Total cost of Schedule
%		-Violated: Constraints violated by Schedule
%
% Holds iff Cost is a total penalty of a Schedule, and Violated is a
% list of soft constraints violated by a Schedule. This procedure
% calls prepare_env/1 if necessary.
cost(schedule(Events), Cost, Violated) :-
	findall(E, class_has_exam(E), Exams),
	findall(P, (teacher(P,_);student(P,_)), Persons),
	prepare_env(Exams),
	person_loop(Persons, Events, Buffer, Violated),
	length(Persons, Len),
	Cost is Buffer / Len.

% person_loop(+Persons, +Events, -Cost, -Violated)
%		+Persons: List of persons' IDs
%		+Events: List of event/4 terms
%		-Cost: Total penalty
%		-Violated: List of violated constraints
%
% For each person and each event in a current schedule compute penalty
% and a list of violated soft constraints. Return total penalty and total
% list of violated constraints for all persons in a list.
%
% If person is a teacher then call correction_time/6 and event_loop/4.
% Otherwise, if person is a student then call study_time/6 and event_loop/4.
person_loop(Persons, Events, Cost, Violated) :- person_loop(Persons, Events, 0, [], Cost, Violated).

person_loop([], _, Cost, Violated, Cost, Violated).
person_loop([P | Ps], Events, Cost0, Violated0, Cost, Violated) :-
	teacher(P, _),
	event_loop(P, Events, ECost, EViolated),
	sort(3, @>=, Events, SortedEvents),
	ex_season_ends(LastDay),
	correction_time(P, SortedEvents, LastDay, 0, DTL, CorrectionCost),
	NewCost is Cost0 + ECost + CorrectionCost,
	new_constraint_violated(P, DTL, CorrectionCost, Violated0, CorrectionViolated),
	append(CorrectionViolated, EViolated, NewViolated),
	!,
	person_loop(Ps, Events, NewCost, NewViolated, Cost, Violated).
person_loop([P | Ps], Events, Cost0, Violated0, Cost, Violated) :-
	student(P, _),
	event_loop(P, Events, ECost, EViolated),
	sort(3, @=<, Events, SortedEvents),
	ex_season_starts(FirstDay),
	study_time(P, SortedEvents, FirstDay, 0, DTL, StudyCost),
	NewCost is Cost0 + ECost + StudyCost,
	new_constraint_violated(P, DTL, StudyCost, Violated0, StudyViolated),
	append(StudyViolated, EViolated, NewViolated),
	!,
	person_loop(Ps, Events, NewCost, NewViolated, Cost, Violated).

% new_constraint_violated(+PID, +DTL, +Cost, +Violated, -NewViolated)
%		+PID: person ID
%		+DTL: How many days did it lack to finish studying or correcting
%		+Cost: Total penalty cause by DTL
%		+Violated: Current list of violated constraints
%		-NewViolated: Extended list of violates constraints
%
% DTL is a number of days a person lacks to finish his or her work, be it
% a study work for a student, or a correcting for a teacher. In case DTL = 0
% then Cost = 0 and a list of violated constraints does not extend, meaning
% NewViolated = Violated. Otherwise, if PID is a teacher's ID then a list of
% violated constraints is extended with c_correction_time(P, DTL, Cost) constraint.
% If PID is a student's ID then Violated is added with c_study_time(P, DTL, Cost)
% constraint.
new_constraint_violated(_, 0, 0, Violated, Violated).
new_constraint_violated(P, DTL, Cost, Violated, [c_correction_time(P, DTL, Cost) | Violated]) :-
	teacher(P, _), !.
new_constraint_violated(P, DTL, Cost, Violated, [c_study_time(P, DTL, Cost) | Violated]) :-
	student(P, _), !.

% event_loop(+PID, +Events, -Cost, -Violated)
%		+PID: Person ID
%		+Events: List of events
%		-Cost: Constraint penalty
%		-Violated: Violated constraints
%
% For each event in a list checks if person is linked somehow with it. If so, the
% following constraints are checked:
% - no_exam_in_period
% - lunck_break
% - not_in_period
% - no_exams_same_day
% - no_exams_in_row
%
% The total penalty of violated constraints computed as Cost, and all violated
% constraints are added to Violated list.
event_loop(P, Events, Cost, Violated) :- event_loop(P, Events, 0, [], Cost, Violated).

event_loop(_, [], Cost, Violated, Cost, Violated).
event_loop(P, [event(Ex, Room, Day, Time) | Evs], Cost0, Violated0, Cost, Violated) :-
	relates_to(P, Ex),
	no_exam_in_period(P, event(Ex, Room, Day, Time), Cost1, Violated1),
	lunch_break(P, event(Ex, Room, Day, Time), Cost2, Violated2),
	not_in_period(P, event(Ex, Room, Day, Time), Cost3, Violated3),
	same_day(P, event(Ex, Room, Day, Time), Evs, Cost4, Violated4),
	in_a_row(P, event(Ex, Room, Day, Time), Evs, Cost5, Violated5),
	NewCost is Cost0 + Cost1 + Cost2 + Cost3 + Cost4 + Cost5,
	append(Violated1, Violated0, V1),
	append(Violated2, V1, V2),
	append(Violated3, V2, V3),
	append(Violated4, V3, V4),
	append(Violated5, V4, NewViolated),
	!,
	event_loop(P, Evs, NewCost, NewViolated, Cost, Violated).
event_loop(P, [_ | Evs], Cost0, Violated0, Cost, Violated) :-
	event_loop(P, Evs, Cost0, Violated0, Cost, Violated).

% correction_time(+PID, +Events, +LastDay, +CorrectionDays, -DTL, -Cost)
%		+PID: Person ID
%		+Events: List of events sorted descending on Day.
%		+LastDay: Day of previously encountered exam.
%			Initialized with last day of exam period
%		+CorrectionDays: Number of days person with PID
%			has left to correct exams. Initialized
%			with 0.
%		-DTL: Number of days teacher lacks to correcting
%		-Cost: Total penalty. Equals to DTL * Penalty
%
% For each event if person is a teacher and is linked to the event then check whether the
% teacher had enough days to do correcting exams. To this end a number of days the teacher
% had is computed, and a number of days the teacher needed is queried. D is the difference.
% In case D is positive or zero, the teacher had enough time to do correcting work. If so,
% go to the next event in the list, replace LastDay with a day when the current event took
% place, and replace CorrectionDays with the number of correction days left. Since events are
% sorted descending, these spare days could be used to correct other exams.
%
% In case teacher did not have enough time to correct exams compute total penalty by multiplying
% a one-day penalty to the number of days: OneDayPenalty * DTL.

correction_time(P, Events, LastDay, CorrectionDays, DTL, Cost) :-
	correction_time(P, Events, LastDay, CorrectionDays, 0, 0, DTL, Cost).

correction_time(_, [], _, _, DTL, Cost, DTL, Cost).
correction_time(P, [event(Ex, _, Day, _) | Evs], LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost) :-
	teaches(P, Ex),
	DaysAvailable is CorrectionDays + (LastDay - Day),
	c_correction_time(Ex, DaysNeeded),
	Delta is DaysAvailable - DaysNeeded,
	DaysLeft is max(0, Delta),
	PenaltyDays is min(0, Delta),
	c_not_enough_correction_penalty(P, Penalty),
	NewCost is Cost0 + (Penalty * abs(PenaltyDays)),
	NewDTL is DTL0 + abs(PenaltyDays),
	!,
	correction_time(P, Evs, Day, DaysLeft, NewDTL, NewCost, DTL, Cost).
correction_time(P, [_ | Evs], LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost) :-
	correction_time(P, Evs, LastDay, CorrectionDays, DTL0, Cost0, DTL, Cost).

% study_time(+PID, +Events, +FirstDay, +StudyDays, -DTL, -Cost)
%		+PID: Person ID.
%		+Events: List of events sorted ascending on Day.
%		+FirstDay: Day of previously encountered exam.
%			Initialized with first day of exam period
%		+StudyDays: Number of days person with PID
%			has left to study. Initialized with 0.
%		-DTL: Number of days student has too little
%			to study
%		-Cost: Total penalty. This equals DTL * Penalty
%
% For each event if person is a student and is linked to the event then check whether the
% student had enough studying days. To this end a number of days the student had is computed,
% and a number of days he or she needed is queried. D is the difference. In case D is positive
% or zero, then student had enough time to study. If so, go to the next event in the list,
% replace FirstDay with a day when the previous event took place, and replace StudyDays with
% the number of study days left. Since events are sorted ascending, these spare days could be
% used to study for the next exam.
%
% In case student did not have enough time to study compute total penalty by multiplying
% a one-day penalty to the number of days: OneDayPenalty * DTL.

study_time(P, Events, FirstDay, StudyDays, DTL, Cost) :-
	study_time(P, Events, FirstDay, StudyDays, 0, 0, DTL, Cost).

study_time(_, [], _, _, DTL, Cost, DTL, Cost).
study_time(P, [event(Ex, _, Day, _)|Evs], FirstDay, StudyDays, DTL0, Cost0, DTL, Cost) :-
	st_group(Ex, Students),
	member(P, Students),
	DaysAvailable is StudyDays + (Day - FirstDay),
	c_study_time(Ex, DaysNeeded),
	Delta is DaysAvailable - DaysNeeded,
	DaysLeft is max(0, Delta),
	PenaltyDays is min(0, Delta),
	c_not_enough_study_penalty(P, Penalty),
	NewCost is Cost0 + (Penalty * abs(PenaltyDays)),
	NewDTL is DTL0 + abs(PenaltyDays),
	!,
	study_time(P, Evs, Day, DaysLeft, NewDTL, NewCost, DTL, Cost).
study_time(P, [_|Evs], FirstDay, StudyDays, DTL0, Cost0, DTL, Cost) :-
	study_time(P, Evs, FirstDay, StudyDays, DTL0, Cost0, DTL, Cost).

% no_exam_in_period(+PID, +Event, -Cost, -Violated)
%		+PID: Person ID
%		+Event: List of events
%		-Cost: Resulting penalty
%		-Violated: Violated constraints
%
% When Event falls in period not preferred by a
% person, Cost = Penalty and Violated =	c_no_exam_in_period/5 term.

no_exam_in_period(P, event(_,_,Day,Start), Penalty, [c_no_exam_in_period(P,Day,From,Till,Penalty)]) :-
	c_no_exam_in_period(P, Day, From, Till, Penalty),
	TillMin is Till - 1,
	between(From,TillMin,Start).
no_exam_in_period(P, event(Ex,_,Day,Start), Penalty, [c_no_exam_in_period(P,Day,From,Till,Penalty)]) :-
	c_no_exam_in_period(P, Day, From, Till, Penalty),
	exam_duration(Ex, Duration),
	End is Start + Duration,
	FromPlus is From + 1,
	between(FromPlus,Till,End).
no_exam_in_period(_,_,0,[]).

% lunch_break(+PID, +Event, -Cost, -Violated)
%		+PID: Person ID
%		+Event: event/4 functor
%		-Cost: Resulting penalty
%		-Violated: Violated constraints
%
% When Event falls during lunch break and a person does not prefer this,
% Cost = Penalty and Violated = c_lunch_break/2 term. When this is not
% the case, Cost = 0 and Violates = []

lunch_break(P, event(Ex,_,_,Start), Penalty, [c_lunch_break(P,Ex,Penalty)]) :-
	Start == 12,
	c_lunch_break(P,Penalty).
lunch_break(P, event(Ex,_,_,Start), Penalty, [c_lunch_break(P,Ex,Penalty)]) :-
	exam_duration(Ex, Duration),
	End is Start + Duration,
	End == 13,
	c_lunch_break(P, Penalty).
lunch_break(_, _, 0, []).

% not_in_period(+PID, +Event, -Cost, -Violated)
%		+PID: Person ID
%		+Event: event/4 functor
%		-Cost: Resulting penalty
%		-Violated: Violated constraints
%
% When Event falls within period not preferred by
% person, Cost=Penalty and Violates=c_not_in_period/6 term.

not_in_period(P, event(Ex,_,Day,Start), Penalty, [c_not_in_period(P,Ex,Day,From,Till,Penalty)]) :-
	c_not_in_period(P,Ex,Day,From,Till,Penalty),
	TillMin is Till - 1,
	between(From,TillMin,Start).
not_in_period(P, event(Ex,_,Day,Start), Penalty, [c_not_in_period(P,Ex,Day,From,Till,Penalty)]) :-
	c_not_in_period(P,Ex,Day,From,Till,Penalty),
	exam_duration(Ex, Duration),
	End is Start + Duration,
	FromPlus is From + 1,
	between(FromPlus,Till,End).
not_in_period(_, _, 0, []).

% same_day(+PID, +Event, +RestEvents, -Cost, -Violated)
%		+PID: Person ID
%		+Event: event/4 functor
%		+RestEvents: List of event/4 functors
%		-Cost: Resulting penalty
%		-Violated: Violated constraints
%
% Loops over RestEvents. When Event and element of
% RestEvents are on the same day, Cost is incremented
% with a penalty and an c_same_day/4 functor is
% appended to Violated. When this condition is not met
% the next element in the list is checked. This uses an
% accumulator.
same_day(P, Event, OtherEvents, Cost, Violated) :-
	same_day(P, Event, OtherEvents, 0, [], Cost, Violated).

same_day(_, _, [], Cost, Violated, Cost, Violated).
same_day(P, event(Ex,Room,Day,Start), [event(Ex2,_,Day,_)|Evs], Cost0, Violates0, Cost, Violated) :-
	relates_to(P, Ex2),
	c_no_exams_same_day(P, Penalty),
	NewCost is Cost0 + Penalty,
	append([c_same_day(P, Ex2, Ex, Penalty)], Violates0, NewViolates),
	!,
	same_day(P, event(Ex,Room,Day,Start), Evs, NewCost, NewViolates, Cost, Violated).
same_day(P, event(Ex,Room,Day,Start), [_|Evs], Cost0, Violates0, Cost, Violated) :-
	same_day(P, event(Ex,Room,Day,Start), Evs, Cost0, Violates0, Cost, Violated).

% in_a_row(+PID, +Event, +RestEvents, -Cost, -Violated)
%		+PID: Person ID
%		+Event: event/4 functor
%		+RestEvents: List of event/4 functors
%		-Cost: Resulting penalty
%		-Violated: Violated constraints
%
% Loops over RestEvents. When Event and element of
% RestEvents are back 2 back, i.e. the same day and
% consecutively, Cost is incremented with a penalty
% and an c_in_a_row/4 functor is appended to Violated.
% When the conditions are not met, next element in
% the list is checked. This uses an accumulator.
in_a_row(P, Event, OtherEvents, Cost, Violated) :-
	in_a_row(P, Event, OtherEvents, 0, [], Cost, Violated).

in_a_row(_, _, [], Cost, Violated, Cost, Violated).
in_a_row(P, event(Ex,Room,Day,Start1), [event(Ex2,_,Day,Start2)|RestEvents], Cost0, Violates0, Cost, Violated) :-
	exam_duration(Ex,Duration),
	Start2 is Start1 + Duration,
	relates_to(P, Ex2),
	c_no_exams_in_row(P, Penalty),
	NewCost is Cost0 + Penalty,
	append([c_in_a_row(P,Ex2,Ex,Penalty)], Violates0, NewViolates),
	!,
	in_a_row(P, event(Ex,Room,Day,Start1), RestEvents, NewCost, NewViolates, Cost, Violated).
in_a_row(P, event(Ex,Room,Day,Start1), [event(Ex2,_,Day,Start2)|RestEvents], Cost0, Violates0, Cost, Violated) :-
	exam_duration(Ex2,Duration),
	Start1 is Start2 + Duration,
	relates_to(P, Ex2),
	c_no_exams_in_row(P, Penalty),
	NewCost is Cost0 + Penalty,
	append([c_in_a_row(P,Ex2,Ex,Penalty)], Violates0, NewViolates),
	!,
	in_a_row(P, event(Ex,Room,Day,Start1), RestEvents, NewCost, NewViolates, Cost, Violated).
in_a_row(P, event(Ex,Room,Day,Start), [_|RestEvents], Cost0, Violates0, Cost, Violated) :-
	in_a_row(P, event(Ex,Room,Day,Start), RestEvents, Cost0, Violates0, Cost, Violated).
