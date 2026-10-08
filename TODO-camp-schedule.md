# Camp schedule tool: product plan

Scope: turn the current camp page (`/termin/[id]` for `type = camp`) into a complete
scheduling workspace. This is a planning document, not a spec. It records what exists,
what is missing, and a phased path, so that each phase ships value on its own.

## 1. What exists today

### Data model (all built on `event_instance`)

- A camp is an `event_instance` of type `camp` spanning several days. Its scheduled
  lessons are child instances (`parent_id`) of type `lesson` (capacity 1 registration)
  or `group` (společná). `event_series` is a separate grouping and is not used for camps.
- Trainers: `event_instance_trainer` on the camp. `lessons_offered` drives requests:
  `0` = does not take requests, `NULL` = unlimited, `N` = cap on total requested lessons.
- Registrations: `event_instance_registration`, one root row per person or couple plus
  child rows per couple member. `note` carries free text (diet, arrival). `source`
  records self / manager / cohort origin. Attendance lives on the person rows.
- Lesson requests: `event_lesson_demand` (registration × camp trainer × count), guarded by
  `set_lesson_demand` against the trainer's cap.
- Conflicts: `event_overlaps_trainer_report` and `event_overlaps_attendee_report` are
  post-hoc SQL reports for a time range. No room conflict report exists.
- Money: `create_event_instance_payment` prices a `lesson` from the trainer's
  `member_price_45min`, pro-rated by duration and split by participant count, and
  `app_private.create_latest_lesson_payments` runs it nightly for lessons that already
  started, then `resolve_payment_with_credit` posts the amounts: debtors' person accounts
  are debited, the trainer account is credited with the payout share and the club account
  with the rest. `account_balances` nets postings per account. The nightly job is
  hard-coded to tenant 2.
- Sharing: `share_token` plus `has_public_details` expose the camp and all children to
  anonymous viewers through `event_share_claims`.
- Permissions: `can_trainer_edit_instance` walks the parent chain, so camp trainers can
  edit any child lesson. `manager_person_ids` is derived from the parent.

### UI

- Camp page tabs: Rozpis (`CampSchedule`), Info, Přihlášky, Lekce (`CampLessonsTable`,
  registrant × trainer matrix with price estimate), Trenéři (`CampTrainersTable`,
  trainer × day counts with payout estimate), Platby.
- `CampSchedule` wraps the generic `Calendar` bounded to the camp dates, forces
  group-by-trainer, and adds a side panel of requests. Dragging a request onto a trainer
  column calls `save_events` to create a 45-minute child lesson with that registrant.
  Dragging a lesson back into the panel deletes it. Move and resize go through
  `move_event_instance`, which also re-assigns the trainer or room for single-trainer
  lessons. Slot selection opens the full `CreateEventForm` with `parentId` preset.
- Registration dialog lets a member or manager pick a person or couple, write a note,
  and set lesson counts per trainer with a live "zbývá" limit.
- Exports: registrations (with requests per trainer) and participants as xlsx.
- `AddToEventScheduleForm` attaches any top-level event to a camp as a child.

### Gaps that shape the plan

1. No draft state. A lesson is visible to members the moment it is dropped, so a
   half-built schedule is public. The share link also ignores `is_visible` on children.
2. Conflicts are only reported after the fact. There is no feedback while dragging and
   no room double-booking check at all.
3. No availability inputs: trainers' arrival, departure, breaks, daily caps; participants'
   arrival and departure. `lessons_offered` caps requests, not scheduled lessons.
4. Rooms are free text per lesson. A camp has no list of rooms, so the room view only
   shows columns already in use and cannot act as a drop target before then.
5. Scheduling is one drop per lesson. Nothing helps place forty requests quickly:
   no place-N-times, no copy-day, no proposal, no undo.
6. Participants are not told about changes. The personal agenda exists (`/rozpis` with
   "Pouze moje", scope `mine`) and already includes camp lessons through the per-person
   child registration rows, but the camp page does not lead there, nothing notifies on a
   change, and there is no printable sheet for trainers or rooms. An ICS feed exists as a
   draft, but calendar apps, Android above all, refresh subscribed feeds on their own
   schedule (hours, not minutes), so it cannot carry same-day changes.
7. Money: the SQL ledger is complete. `create_event_instance_payment` prices a lesson,
   `resolve_payment_with_credit` posts it to the debtors' accounts and splits the trainer
   payout from the club share, and `account_balances` nets everything per person. What is
   missing for camps is narrow: the tenant 2 hard-code, pricing for `group` lessons,
   camp-specific rates, guest rates, and a per-camp statement. The Lekce and Trenéři tabs
   re-implement the price estimate in TypeScript instead of reading it from SQL.
8. External registrations cannot request or receive lessons.
9. Privacy: `view_visible_instance` lets any member read every registration's note and
   lesson requests on a visible camp. Notes often contain diet or health details.
10. HTML5 drag and drop does not work on touch devices, so tablets cannot schedule.

## 2. Strategy

The job to be done: turn N requests (registrant × trainer × count) into a conflict-free
timetable across days, trainers and rooms, under availability constraints, then publish
it, keep it current during the camp, and settle money afterward.

Principles:

- Manual first, automation as a proposal. Organizers will not trust a black box. Any
  auto-fill must render as a preview the organizer accepts or edits.
- Capture constraints before building a solver. A solver without availability data
  produces timetables nobody can use.
- Keep lessons as child `event_instance` rows. That keeps attendance, payments, conflicts,
  the calendar, RLS and sharing working without a parallel data model.
- Prefer additive schema changes that reuse existing columns and event types. Reach for
  new tables only when a concept needs its own identity.
- Each phase must be usable on its own at the next camp.

Users and their primary screens:

| User | Needs | Screen |
| --- | --- | --- |
| Organizer (admin or head trainer) | build, publish, adjust, settle | Rozpis, Lekce, Trenéři, Platby |
| Trainer | own day sheet, attendance | Rozpis filtered to self, print |
| Participant or parent | register, request, see own lessons, hear about changes | Přihlášky dialog, personal agenda ("Pouze moje"), share link |

## 3. Phases

### Phase 0: harden the existing flow

Goal: the next camp can be scheduled with confidence using what exists. Small, mostly
frontend changes.

- Draft and publish. Create child lessons with `is_visible = false` while the camp's
  schedule is unpublished, add a camp action "Zveřejnit rozpis" that flips all children,
  and filter `event_share_claims` by `is_visible`. Publish state is derived from the
  children, so no new column. Members and the share link then see only published lessons.
- Live conflict feedback while dragging. The range view already holds every event of the
  camp in memory, so highlight the target slot as busy when the trainer, any participant
  (including each couple member), or the room already has a lesson there. Keep the SQL
  reports as the source of truth at publish time.
- Room conflict report in SQL, mirroring the trainer report, so the conflicts dialog and
  the instance badge cover rooms.
- Undo for move, resize, create and delete, as a client-side stack replaying
  `move_event_instance`, `save_events` and `deleteEventInstance`.
- Requests panel usability: sort unfulfilled first, filter by trainer, search by name,
  per-trainer header totals (requested / scheduled / offered), collapse fulfilled rows.
- Refetch on window focus and after every mutation, so two organizers do not schedule
  on stale data.
- Fix the lesson created from a request to inherit the camp's visibility flags instead of
  the `save_events` defaults.

### Phase 1: capture constraints

Goal: the data the scheduler and the conflict checks need. One small migration.

- Trainer blocks. Use existing `reservation` child instances with the trainer attached as
  "Přestávka", "Příjezd", "Odjezd" blocks. They already show in the trainer column and
  count in the trainer conflict report. Add a quick "Přidat blok" action on the column
  header, render them as background events, and create them with `is_visible = false`
  so members never see them. No schema change.
- Participant availability. Add nullable `arrives_at` and `leaves_at` to
  `event_instance_registration`, editable in the registration form for camps. Treat
  time outside the window as busy in live feedback and in the attendee conflict report.
- Camp rooms and defaults. Add `event_instance.schedule_settings jsonb` validated by a
  check constraint, holding `rooms[]`, `lesson_minutes`, `day_start`, `day_end`. Lessons
  keep using `location_text` as the room key, so no new foreign keys. The room view then
  shows every room as a fixed column and the drag preset picks the configured duration.
- Per-trainer daily cap. Add `max_lessons_per_day` to `event_instance_trainer` as a soft
  limit surfaced in the Trenéři tab and in live feedback.
- Restrict `event_lesson_demand` and `event_instance_registration.note` reads to the
  registrant and managers. Do this before widening any participant-facing feature.

### Phase 2: scheduling productivity

Goal: an organizer places a full camp in an afternoon.

- Click-to-place mode: select a request, then click slots to place it repeatedly. This
  also makes tablets usable, since it needs no HTML5 drag and drop.
- Place-N-times: drop a request with count 3 and fill three consecutive free slots for
  that trainer on that day, skipping blocks and participant conflicts.
- Swap two lessons (two `move_event_instance` calls, no modeling). Day templates are
  deliberately out: copying layouts between days would need its own model for little
  gain over placing blocks by hand.
- Proposal fill. A greedy heuristic (client-side first, worker later if it grows) that
  takes unfulfilled demands and proposes placements honoring blocks, arrival and departure,
  rooms, daily caps, and that favors back-to-back lessons for a registrant and spreads a
  registrant's lessons with one trainer across days. Render through the existing
  `__isPreview` path, let the organizer accept all, accept some, or discard. Commit in one
  `save_events` call.
- Highlight-on-click: clicking a trainer or registrant name dims everything else, using
  the existing trainer and participant filters.

### Phase 3: communication and the participant experience

Goal: nobody has to ask "when is my lesson?"

- One personal agenda, not a camp-specific list. The existing `/rozpis` agenda with
  "Pouze moje" already returns club lessons and camp lessons together. Make it the
  canonical "my lessons" view: link to it from the camp page and the registration dialog,
  and group camp lessons under their camp heading in the agenda so a member sees Monday's
  club lesson and the weekend camp in one list.
- Change notifications after publish. On create, move or cancel of a published lesson,
  record the affected person ids and send one digest per person through the worker, with
  an explicit "Odeslat změny" action so a burst of edits produces one message. Deliver by
  web push first: the service worker already handles push payloads, so what is missing is
  subscription storage and a sender task. Email is the fallback for people without a
  subscription.
- ICS stays a convenience export, not the delivery channel. Calendar apps poll feeds on
  their own interval, so the feed is right for next week's plan and wrong for a lesson
  moved this morning. The agenda plus push covers the latter.
- Printable sheets: per trainer per day, per room per day, and per registrant, with a
  print stylesheet for the time grid. Paper on the hall door is still how camps run.
- Share link respects visibility (from Phase 0) and gets an optional per-trainer read-only
  variant.

### Phase 4: money and after-camp

Goal: the camp settles through the existing ledger with no spreadsheet. Per-lesson
payments and per-person account balances already consolidate correctly, so there is no
need for a separate camp invoice.

- Extend the SQL pricing, not the TypeScript. Teach `event_instance_approx_price` and
  `create_event_instance_payment` about `group` lessons and about per-camp rate overrides
  (per-trainer `price_45min` and `payout_45min` on `event_instance_trainer`, falling back
  to `tenant_trainer`). Then make the Lekce and Trenéři tabs read `approxPriceList` and a
  matching payout field instead of recomputing, so estimates and postings agree.
- Remove the tenant 2 hard-code from the nightly job and from
  `create_event_instance_payment`, replacing it with a per-tenant setting.
- Per-camp statement: a view or function summing postings of lessons under the camp per
  account, giving each registrant's camp total and each trainer's payout from the same
  rows the ledger already holds. Export it for trainers.
- Camp attendance grid (registrant × lesson). Decide per camp whether a no-show is still
  billed; the existing cancellation trigger already drops the payment for cancelled
  lessons.
- Guest rates for external participants once Phase 5 gives them lessons.

### Phase 5: broader scope

- External guests with lessons: on registration, create a stub `person` outside tenant
  membership so demand and scheduling work unchanged.
- Clone camp: copy trainers, rooms, settings and caps from a previous camp.
- Cross-camp analytics: demand per trainer, fulfillment rate, utilization per room and
  hour, to plan trainer invitations for the next camp.
- Participant self-service change requests (swap or cancel a lesson) with organizer
  approval, only if change traffic after publish turns out to be high.

## 4. Sequencing and dependencies

- Phase 0 first: it is small and it removes the two biggest risks at the next camp,
  accidental exposure of a draft and undetected overlaps.
- Phase 1 before Phase 2: the proposal fill and place-N-times are only as good as the
  constraints they know about.
- Phase 3 can run in parallel with Phase 2. Its only prerequisite is the publish step.
- Phase 4 after the schedule is stable, since billing depends on the final timetable and
  on attendance.

## 5. Measures of success

- Hours from registration close to published schedule.
- Minutes from a lesson change to the affected participants knowing about it.
- Conflicts reported at publish (target zero).
- Share of requests fulfilled at publish.
- Lessons changed after publish and messages sent per change.
- Questions to the organizer during the camp about times and rooms.

## 6. Open questions for the maintainer

1. How many camps per year, and which tenants run them? This decides how much of Phase 4
   must be tenant-generic from the start.
2. Typical size: trainers, couples, days, rooms. A 6 × 30 × 3 camp needs Phase 2; a
   3 × 10 × 2 camp may stop after Phase 1.
3. Are lessons always 45 minutes? Do group lessons come from requests or are they set by
   the organizer?
4. Do participants pay per lesson, a camp fee, or both? Is the camp fee ever to be modeled
   here, or does it stay outside?
5. Do external guests need lessons?
6. Who schedules: one organizer, or each trainer their own column? This decides whether
   concurrent editing needs more than refetch-on-focus.
7. Is a print sheet on the hall door still how the camp runs on site?
