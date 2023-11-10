/*Один из недостатков предыдущей системы является, то что все
 * факты пользователь должен утвердить заранее.
 * Отсюда появляются сразу несколько минусов, что пользователь
 * может утвердить слишком много фактов и будут ненужные или слишком
 * мало и будет нехватка. Поэтому было бы лучше, если бы информация была
 * предоставлена в ходе диалога.
 *
 * От экспертной системы требуется объяснить как она добралась до того
 * или иного результата.
 *
 * Когда система предоставляет ответ, то пользователь должен иметь
 * возможность спросить: "Как ты пришел к такому ответу?". Обычно
 * объяснение заключается в предоставлении пользователю следа,
 * полученного ответа.
 * 
 * Пример: система обнаружила, что есть утечка на кухне и пользователь
 * спрашивает "Как?".
 *
 * Объяснение:
 * (а) Есть проблема на кухне, это полученно из того что
 *                             коридор влажный и ванная.
 *  И
 * (b) Вода не поступала из вне, что следует из
 *                                          окно закрыто.
 *
 * Это объяснение представляет собой дерево доказательств того, как
 * окончательный вывод следует из правил и известных фактов.
 *
 * Мы можем выбрать представление дерева доказательств предложения P в
 * одной из следующих форм.
 *
 * (1)Если P - это факт, то весь путь доказательства - это сам факт P
 * (2)Если З было выведено с использование правила (IF ... THEN P), то
 * дерево доказательств представлет собой P <== ДОКАЗАТЕЛЬСТВО УСЛОВИЯ
 * (3)Допустим, что P1 и P2 - это предложения, деревья доказательства
 * для которых это PT1 и PT2.
 *
 * Если P - это P1 и P2, то дерево
 * доказательства - это PT1 и PT2.
 *
 * Если P - это P1 или P2, то дерево
 * доказательства - это PT1 или PT2.
 */


/*Оператор dynamic используется для динамического изменения предикатов во
 *время выполнения программы
 */
:- dynamic derived/1.
:- dynamic asked/1.
/*Оператор op определяет новые операторы, а также
 * устанавливает для них тип и приоритет.
 */
:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

/*
 * Очищает все факты derived и asked в начале компиляции.
 * Делает для того, чтобы при перекомпиляции программы не
 * оставалось фактов с прошлых компиляций и не возникало
 * некорректного поведения программы.
 */
:- retractall(derived(_)), retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*Система диагностики какого-либо заболевания
 *
 *
 */

%1.Если заложен нос и слизь -> насморк
if
    i_have_a_stuffy_nose and i_have_mucus_from_my_nose
then
    i_have_a_runny_nose.


%2.Если насморк и чихание и зуд -> аллергия
if
    i_have_a_runny_nose and i_am_sneezing and i_have_an_itch
then
    i_have_allergies.

%3.Если сыпь на коже и зуд -> часотка
if
    i_have_an_itch and i_have_a_rus_on_my_skin
then
    i_have_scabies.


%4.Если на коже пустулы или фликтены и часотка -> пиодермия
%Пустулы и фликтены особые воспаления кожи.
%Пиодермия осложнение часотки.
if
    i_have_scabies and i_have_pustules_or_flickens_on_my_skin
then
    i_have_pyoderma.

%5.Если насморк и чихание и усталость и температура -> ОРВИ
if
    i_have_a_runny_nose and i_am_sneezing and i_have_fatigue and i_have_a_fever
then
    i_have_ARVI.

%6.Симпоты ОРВИ и гной в горле -> тонзиллит
if
    i_have_pus_in_my_throat and i_have_ARVI
then
    i_have_tonsillitis.

%7.Усталость и нету аппетита и кашель и потеря веса -> рак легких
if
    i_have_fatigue and i_have_no_appetite and i_have_cought and i_have_weight_loss
then
    i_have_lung_cancer.

%8. Синдромы орви + нету запахов или вкусов -> ковид
if
    i_have_ARVI and (i_cant_smell or i_cant_taste_the_food)
then
    i_have_a_caronovirus.
%9.усталость + температура + озноб + интоксикация -> грипп
if
    i_have_fatigue and i_have_a_fever and i_have_chills and i_am_intoxicated
then
    i_have_the_flu.

%10. все симпотмы грипа и болят суставы и кашель без крови -> бронхит
if
    i_have_the_flu and my_joints_hurt and i_have_a_cough_without_blood
then
    i_have_bronchitis.

%11. отдышка и кашель и сдавленность в груди -> астма
if
    i_am_short_of_breath and i_have_cought and i_feel_pressure_in_my_chest
then
    i_have_asthma.

%12. астма + приступы удушья -> тяжелая бронхиальная астма
if
    i_have_asthma and i_have_constant_attacks_of_suffocation
then
    i_have_severe_bronchial_asthma.

%13. головные боли + повышенная чувствительность к свету -> мигрень.
if
    i_have_a_headache and i_have_an_increased_sensitivity_to_light
then
    i_have_a_migraine.
% 14. У меня болят суставы или жжение в области суставов или утрення
% скованноть -> артрит
if
    my_joints_hurt or i_have_a_burning_sensation_in_my_joints or i_have_morning_stiffness
then
    i_have_arthritis.

%15. Боль в глазах + головные боли + гало вокрух света -> глаукома
if
    i_have_pain_in_my_eyes and i_have_a_headache and i_have_a_halo_around_the_light_sources
then
    i_have_glaucoma.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*Некоторое количество легких симпотом, которым ничего не предшевствует.
 * Исходя из данных симпотом, система будет определять более тяжёлые
 * заболевания, которые как раз таки и совпровождаются некоторым
 * количеством ниже-перечисленных симптомов.
 */
askable(i_have_cought).% у меня есть кашель
askable(i_have_a_stuffy_nose).% у меня заложен нос
askable(i_have_mucus_from_my_nose).% у меня выделяется слизь из носа
askable(i_am_sneezing).%я постоянно чихаю
askable(i_have_an_itch).%зуд
askable(i_have_a_rus_on_my_skin).%сыпь на коже
askable(i_have_pustules_or_flickens_on_my_skin).%У меня на коже пустулы или фликтены
askable(i_have_fatigue).%У меня усталость
askable(i_have_a_fever).%У меня температура
askable(i_have_pus_in_my_throat).%у меня гной в горле
askable(i_have_no_appetite).%у меня нет аппетита
askable(i_have_weight_loss).%у меня потеря веса
askable(i_cant_smell). %я не чувствую запахи
askable(i_cant_taste_the_food). %я не чувствую вкус пищи
askable(i_have_chills). % у меня озноб
askable(i_am_intoxicated). % у меня интоксикация
askable(my_joints_hurt).% у меня болять суставы
askable(i_have_a_cough_without_blood). % мой кашель без крови
askable(i_am_short_of_breath). %у меня отдышка
askable(i_feel_pressure_in_my_chest). % я чувствую давление в груди
askable(i_have_constant_attacks_of_suffocation). % у меня постоянные приступы удушья
askable(i_have_a_headache). % у меня головная боль
askable(i_have_an_increased_sensitivity_to_light). % у меня повышенная чувствительность к свету
askable(i_have_a_burning_sensation_in_my_joints). % у меня чувство жжения в суставах
askable(i_have_morning_stiffness). % у меня утренняя скованность
askable(i_have_pain_in_my_eyes). % у меня боль в глазах.
askable(i_have_a_halo_around_the_light_sources). % гало вокруг источников света

true(Statement, Proof) :-
    retractall(derived(_)),
    retractall(asked(_)),
    true(Statement, Proof, []).

true(Statement, Statement, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
    true(S1, P1, Trace),
    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
    true(S1, P, Trace) ;
    true(S2, P, Trace).
true(Conclusion, Conclusion <== ConditionProof, Trace) :-
    if Condition then Conclusion,
    true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-
    askable(Statement),
    \+ derived(Statement),
    \+ asked(Statement),
    ask(Statement, Proof, Trace).


ask(Statement, Proof, Trace) :-
    format('\nIs it true that ~w ? Please answer \'yes\', \'no\' or \'why\'.\n',[Statement]),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, Statement, Proof, Trace).


process("yes", S, S <== was_told, _) :- !,
    asserta(derived(S)),
    asserta(asked(S)).
process("no", S, _, _) :-   !,
    asserta(asked(S)),
    fail.
process("why", Statement, Proof, Trace) :-  !,
    show_reasoning_chain(Trace, 0), nl,
    ask(Statement, Proof, Trace).
process(_, Statement, Proof, Trace) :-
    write('Please answer only \'yes\', \'no\' or \'why\'!\n'),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, Statement, Proof, Trace).


show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
    format('\n   To infer ~w, using rule\n\t   (if ~w then ~w)',
           [Concl, Cond, Concl]), show_reasoning_chain(Rules, _).


demo() :-
    true(i_have_allergies, St),
    format('\n ~w', [St]).
