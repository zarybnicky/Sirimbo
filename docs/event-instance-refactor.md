# Event → EventInstance refactor & camp schedule

## Status (2026-07)

**In progress.** The read path is fully off `event`, and registration is
mid-cutover via a **dual-write bridge** (`event_instance_registration`) rather
than the compat-view swap first envisioned. Where we are:

### Landed on master (through migration `000104`)

- **Instance decoupled from `event_id`, steps 1–4** — `event_id` nullable,
  every inheritable attribute owned on the instance (sync triggers extended and
  backfilled), FKs/uniques re-keyed to `(tenant_id, instance_id)`,
  `can_trainer_edit_instance` + instance-level RLS.
- **Read path off the instance** — the calendar and the per-instance page are
  Event-free; the registration Event is fetched lazily only when the dialog
  opens; the per-instance page lives at `/termin/[instance]` (301 from the old
  `/akce/[id]/termin/[instance]`).
- **Camp scoped-calendar "Program" tab** on `EventView`.
- **Registration dual-write bridge (`000104`).** `event_instance_registration`
  is a **mirror** of the still-authoritative `event_registration` (units) +
  `event_attendance` (person rows), so reads move onto it with every writer
  untouched:
  - **Table** — unit row (couple/solo, `parent_registration_id` null), person
    row (`parent_registration_id` → unit, carries `status`), solo = combined
    row. `@omit`-ted from GraphQL.
  - **Skeleton derived from `event_registration × event_instance`** (what
    `create_attendance` computed), never from `event_attendance` — so the table
    can later become a view over the mirror without a circular read. Status is
    **native**: seeded once from `event_attendance`, `'unknown'` on new rows,
    preserved on conflict, propagated from marks.
  - **Triggers** — `sync_eir_registrations` driven by registration INS/UPD/DEL,
    instance INSERT, instance reparent (`UPDATE OF event_id`, for `detach`), and
    attendance status propagation. Sync upserts in place on the bridge key (ids
    stable), then sweeps orphans.
  - **Reads repointed** — `registrations` / `my_registrations` /
    `remaining_person_spots` filter through the mirror by instance but still
    return `setof event_registration` / `integer`, so ids, cancel, and
    lesson-demand are unchanged. `remaining` excludes `cancelled`.
  - **Constraints** — bridge key `(legacy_registration_id, instance_id,
    coalesce(person_id,-1))` plus natural, bridge-independent `unit_key`
    (mirrors `event_registration`'s unique, re-keyed to the instance) and
    `person_key`.
  - `legacy_registration_id` links each mirror row to its source registration —
    a bridge artifact, dropped when writes flip.

### Landed on master (migration `000105`, B4)

- **B4 — `event_attendance` → read-only view over the mirror's person rows.**
  The mirror becomes the *attendance* source of truth:
  - `refresh_event_instance_stats` counts eir person rows; the stats trigger
    moves onto eir (statement-level for the bulk sync, per-row on `status`).
  - `create_attendance` triggers retired (sync builds the skeleton).
  - `update_event_attendance` marks the eir row directly; `detach` no longer
    juggles attendance manually (sync + the reparent trigger keep it consistent).
  - `event_attendance` becomes a `security_invoker` **view** (`WHERE person_id
    IS NOT NULL`, `legacy_registration_id AS registration_id`), `@omit
    create,update,delete`, PK/FK restored by smart comment. Read-only — no
    `INSTEAD OF`; every writer targets the mirror.
  - **Internal SQL readers repointed** off the view onto the mirror
    (`event_instance_attendance_summary`, `event_instances_for_range`,
    `event_overlaps_*_report`, `scoreboard_entries`), so only the GraphQL
    surface (the `eventAttendance` relations, `activity_timeline`) still depends
    on the view — that moves with the UI next.
  - **Mirror exposed for reads** — `event_instance_registration` un-`@omit`-ted
    for select (`@omit create,update,delete`), `legacy_registration_id` kept
    `@omit`. `activity_timeline_item` gains an unused `eventInstanceRegistration`
    relation (same column as `eventAttendance`, since the ids coincide), ready
    for the UI to switch off `eventAttendance`.
  - Dropped `person_recent_attendance` (unused).
  - The last `event_attendance` **writer** (`tg_cohort_membership__on_status`,
    which cancels future attendance on membership expiry) and the remaining
    readers (`person_weekly_attendance`, `event_registration_last_attended`,
    `trainer_group_attendance_completion`, `detach`'s snapshot) were repointed
    onto the mirror, so no internal SQL still writes/reads the view.
  - **`lastAttended` computed field** on the mirror (mirror of
    `event_registration_last_attended`) and a **parallel `update_attendance`
    writer** (returns `event_instance_registration`) added — the read/write pair
    the migrated attendance UI needs. `update_event_attendance` is **kept** so
    old cached clients keep working through a gradual rollout; both writers mark
    the same eir row.

- **Attendance UI on the mirror.** `InstanceAttendanceView` reads
  `eventInstanceRegistrationsByInstanceId` and `lastAttended` off the mirror and
  writes through `updateAttendance` (graphcache-coherent: read and write are the
  same `EventInstanceRegistration` entity). The `EventAttendance` GraphQL
  fragment is retired from source; old bundles carry their own copy.

### Remaining

- **UI migration onto the mirror (rest)** — the attendance-marking view is done;
  still to move: the registration surfaces (`NewRegistrationForm` /
  `MyRegistration*`), `EventView`'s registration/attendance tabs, and
  `ActivityTimeline`'s event-attendance items (onto the
  `eventInstanceRegistration` relation). Once nothing reads the `event_attendance`
  view or `eventRegistration`, drop those relations / the view.
- **Write-path flip** — `register` / `cancel` / `edit` target the mirror
  directly; drop `event_registration` (→ view or gone) and
  `legacy_registration_id`; promote `unit_key`/`person_key` to the
  source-of-truth constraints.
- **Instance decoupling steps 5–6** — bare-instance authoring writes
  (`upsert_event` / `quick_create_events` create `event_id`-null instances);
  retire `fill_defaults` / `pin_overrides` / `propagate_to_instances`.
- **Schedule authoring** — parent-aware create, `capacity`/`is_locked` in the
  quick forms, `move_event_instance` writes `instance.location` and keeps the
  slot in its camp, `detach_event_instance` → clear `parent_id`.
- **Trainers** — `event_instance_trainer` gains `lessons_offered` and becomes
  the sole trainer table; `event_trainer` retires.
- **External registration** — repoint `event_external_registration` to
  `instance_id`.
- **Cohort targeting** — `event_instance_target_cohort` + forward-bounded
  reconciliation replacing the all-time triggers.
- **Series + drops + rename** — create `event_series`; drop the compat views,
  `event`, `event_target_cohort`, `event_trainer`, the sync/fanout triggers;
  rename `event_instance → event`.

### Notes captured during the bridge

- `event_registration_last_attended` is a registration-level derived field, so
  marking one partner moves the whole registration's "last attended" — cosmetic
  now, but a semantics question for `event_series` (per-instance vs per-series vs
  per-registration).
- Cancelled attendance does not occupy a slot (excluded from remaining/stats),
  but its row stays in the skeleton (still in the registration×instances product).
- On prod, the only registrations with no mirror rows are on events with **zero
  instances** (dead data) — expected; everything with instances is mirrored.

### Why a bridge, not the compat-view cutover

Decoupling the instance structurally and moving the read path first proved the
model without the riskiest migration. The dual-write bridge then keeps every
writer working while reads move, deferring the risky table→view swap: `000104`
mirrored reads, B4 makes the mirror the attendance source and turns
`event_attendance` into a view, and the write-path flip finishes the cutover.
Slice 4c was the only Slice-4 piece on the camp-schedule critical path —
trainers already ride on `event_instance_trainer`, cohort targeting is parked
for camps — so it went before authoring.

## Motivation

`public.event` currently serves two roles at once: a real thing that happens
(a camp, a single lesson) and a recurring series that owns instances. The
`event_instance` is a projection of the event, kept in sync by
`propagate_to_instances` / `fill_defaults` / `pin_overrides`. That conflation
forces every satellite (registration, trainer, cohort) onto the series-shaped
`event` and blocks per-occurrence features — the driving one being a **camp
schedule**, a per-day program of slots inside one camp.

**Direction:** make `event_instance` the entity, add a separate `event_series`
entity for recurrence, and drop `event`. Once `event` is gone, `event_instance`
is renamed to `event` — a different entity than today's, but the natural name.
New columns are named for that end state (hence `parent_id`, a self-FK,
not `parent_instance_id`).

---

# Data model

## `event_instance` is the entity

An instance is a real thing that happens at a time and place. It already carries
`since`/`until`, `name`, `type`, `location_*`, `is_visible`/`is_public`,
`manager_person_ids`, `stats`, and free-form `custom`. It gains:

- `parent_id` — self-FK, **composition**. A camp is a parent instance; its
  schedule slots are child instances.
- `series_id` — FK to `event_series`, **recurrence**.
- `capacity` + `capacity_unit` (`'people'` | `'registrations'`).
- `description`, `summary`, `is_locked`, `enable_notes`, `files_legacy` — moved
  from `event` (no instance home today).

`parent_id` and `series_id` are orthogonal; an instance may have both
(an annual camp). Once `event` is dropped, `is_locked` drives a per-instance
`event_is_registration_open`.

## `event_series`

Its own table, **not** an event type. A synthetic grouping row (identity + bulk
edit metadata) that recurring instances reference via `series_id`. It owns no
operational state — no trainers, registrations, or cohort targeting. It exists
to (1) anchor bulk edits across its instances and (2) back a "register to the
whole series" action.

## Registration (one table, per instance)

`event_instance_registration` unifies today's `event_registration` (the
couple/solo unit) and `event_attendance` (the person rows). The invariant:
`event_registration == event_attendance == event_instance_registration`, one
thing, per instance. Its rows:

- **couple registration** — `couple_id` set, no `person_id`;
- **person registration** — `person_id` set, carrying per-person `status`; a
  partner's row references its parent couple registration via a **self-FK**
  (`parent_registration_id`, replacing today's `event_attendance.registration_id
  → event_registration`);
- **solo registration** — a single person registration with no parent, which is
  at once the couple/solo-level registration and the person row.

Registering a couple creates **three rows** (one couple + two person);
registering a solo creates **one**. `note` and `target_cohort_id` live on the
couple/solo registration. The couple→person expansion is exactly what
`event_registration_person_ids` (couple → `man_id`/`woman_id`) does today inside
the `create_attendance` triggers; that logic moves into the register path when
writes flip.

Registration is per-instance and does not fan out across instances. Registering
to a series or cohort inserts independent per-instance registrations (see Cohort
targeting).

Columns: `id`, `tenant_id`, `instance_id`, `event_id` (kept during the
transition for the FK/RLS the old shape needs; derivable from the instance),
`couple_id`, `person_id`, `parent_registration_id` (self-FK), `status`
(**nullable** — set on person rows, null on couple rows), `note`,
`target_cohort_id`, `legacy_registration_id` (bridge artifact), `created_at`,
`updated_at`. Row kinds by discriminator:

- couple row — `couple_id` set, `person_id`/`parent_registration_id`/`status` null;
- partner person row — `person_id` set, `parent_registration_id` = its couple row,
  `couple_id` null (reached via the parent);
- solo person row — `person_id` set, `parent_registration_id` null.

## Facts

- **Attendance status** — on the person registration. Attendance stats
  (`event_instance.stats`, refreshed by the stats trigger) count **only** person
  registrations (`person_id IS NOT NULL`); couple registrations have no status
  and would otherwise inflate the counts.
- **Lesson demand** (`event_lesson_demand`) — on the couple/solo registration;
  `registration_id` repoints from the old `event_registration`, `trainer_id`
  from `event_trainer` to `event_instance_trainer`.
- **Accommodation** (planned) — on the person registration, with
  copy-from-partner in the UI.

## Trainers & lessons offered

`event_instance_trainer (instance_id, person_id)` becomes the sole trainer
table; `event_trainer` retires. But `event_trainer` carries `lessons_offered`
(a trainer offers N lessons across a camp; lesson demand counts against it, and
`event_trainer_lessons_remaining` computes the balance) — a real feature with no
equivalent on `event_instance_trainer`. So **`event_instance_trainer` gains
`lessons_offered`**, held on the camp's parent instance, and the
lessons-remaining calculation recomputes against it. This is a prerequisite for
retiring `event_trainer`.

Today two resolution rules ride on the `event_trainer` + `event_instance_trainer`
pair and both must survive the collapse: **displayed trainers replace**
(`event_instance_trainers_at` uses an instance override *instead of* the event
trainers) while **manager permissions union** (`event_instance_manager_person_ids`
counts event trainers on every instance regardless). After the collapse,
manage-union comes from the `parent_id` walk for camps; recurring lessons
have no equivalent (series owns no trainers) — **open** (keep or drop).

## Capacity

A single `capacity` integer plus `capacity_unit`:

- `'people'` — counts person registrations (a couple = 2, a solo = 1).
- `'registrations'` — counts couple/solo registrations (a couple = 1, a solo = 1).

Enforcement is out of scope for the additive phase. (Today `EventButton`
encodes this inline as `capacity > totalCount * 2` — capacity in people,
registrations counted as couples; that becomes an instance-level check.)

## Cohort targeting

Cohort targeting applies to group lessons, not camps. The rule "these
occurrences are for cohort X" lives in `event_instance_target_cohort
(instance_id, cohort_id)`, per instance. `event_target_cohort` (keyed on
`event`) is dropped after its rows migrate here.

- Past instances are frozen; their registrations are history.
- Future instances (`since ≥ now`) are reconciled with cohort membership: a
  member joining is registered to future targeted instances, a member leaving is
  removed from them.
- The series generator copies the targeting from an existing series instance
  onto each new future instance, so the series holds no state.

This replaces the reconcile-all-time triggers (`register_members`,
`unregister_members`, `register_new_cohort_member_to_events`, membership
reverse-sync) with a forward-bounded version.

---

# Camp schedule

A camp is a tree of instances joined by `parent_id`. Registration is a
property of individual instances and never flows along the parent/child edge:

- **Camp** — the parent instance, spanning the full date range. Registering to
  the camp is an ordinary registration on this instance.
- **Child slots** — ordinary instances (group or private lessons) that happen to
  carry a `parent_id`. Having a parent changes **nothing** about registration: a
  slot has ordinary registrations, exactly like a standalone lesson.
- `is_locked` gates only whether the **registrant** may change their own
  registration (self sign-up / self-cancel). It does **not** mean the slot has no
  registrations — a locked slot can be fully populated, just managed by
  trainers/admins rather than the members. A "free lesson" is a slot left open
  for members to sign themselves up; a schedule-only slot members can't book is
  simply a locked one.
- Child-slot registration is **fully independent** of camp registration —
  neither requires nor implies the other.

The schedule view is the club calendar scoped to that camp (Frontend §1).

---

# Frontend

GraphQL is derived from the schema by PostGraphile, but the frontend queries are
hand-written (`graphql/Event.graphql`), so every structural change means
rewriting fragments and the components that read them. Three surfaces, at very
different readiness.

## 1. Schedule view — the calendar, scoped to the camp

A camp's schedule is the club calendar scoped to that camp — the same
`useCalendarData` → `Agenda`/`TimeGrid` pipeline, showing only the camp's
children (`event_instance` rows with `parent_id` = the camp).
`event_instances_for_range` takes a scope parameter; `useCalendarData` passes
the scope; the camp page embeds the calendar. **Landed** (Slice 2).

Standalone instances — a club's regular lessons and group lessons — have no
`parent_id` and appear on the main (unscoped) calendar as always. The
scoped calendar is a camp-only view; there is nothing legacy about flat events.

## 2. Authoring — on the scoped calendar

A camp is created like any event (existing paths, unchanged). Its *schedule* is
authored on the scoped calendar (§1), using the calendar's existing affordances:
drag-create (`QuickEventCreateForm` → `quick_create_events`), quick-edit
(`QuickInstanceEditForm` → `update_event_instance_details`, which already writes
per-slot `name`/`type`/`location`/`is_cancelled`/trainers), and drag-move
(`move_event_instance`). Still to do:

- **Hierarchy.** `quick_create_events` makes a *standalone event per slot*;
  within a camp's scoped calendar it must instead create a child instance with
  `parent_id` = the camp.
- **`capacity` + `is_locked`** aren't in the quick forms.
- `move_event_instance` must keep a slot within its camp and write
  `instance.location` (today it writes the event's).

The big form (`upsert_event`) still can't set per-slot fields (its instance loop
writes only `since`/`until`/`is_cancelled`/trainers) — but that path authors the
camp entity, not its schedule, so it isn't on the camp-schedule critical path.

`detach_event_instance` is a 100-line clone-the-whole-event routine; under the
model it collapses to "clear `parent_id`." The `eventInstance.detach`
action ("Oddělit termín") keeps its UX and swaps its mutation.

## 3. Registration — event-scoped end to end (write path still to flip)

`register_to_event_many` checks `event.is_locked` and inserts an event-level
`event_registration`; the `create_attendance` triggers fanned it to per-instance
`event_attendance` (now the sync builds the skeleton, and `event_attendance` is
a view — B4). `cancel_registration`, `edit_registration`, `set_lesson_demand`,
`event_is_registration_open`, and the `delete_my`/`update_my` policies all still
key on the event. The forms (`NewRegistrationForm`, `MyRegistrationForm`,
`MyRegistrationCard`) pass an event.

Flipping to per-instance: register targets an instance (the camp's parent, or a
free-lesson child); the couple→three-rows expansion moves into the register
path; `is_locked` is read on the instance. `MyRegistrationForm`'s lesson demand
reads `event.eventTrainersList` (`event_trainer` + `lessons_offered`) — it moves
to the instance/parent trainers. Attendance is the least affected:
`InstanceAttendanceView` already keys on `(instanceId, personId)` and filters
`keyIsNonNull('person')`, which excludes the couple rows for free.

## Cross-cutting couplings (what still moves with the write path)

- **Event-level `capacity`/`isLocked`/registration-count** in `EventFragment` are
  read by `EventButton` (every calendar cell/list row), `EventSummary`,
  `MyRegistrationsDialog`, `NewRegistrationForm`, `BasicEventInfo`, `EventList`
  (sidebar subtitle), and `EventView`. The *read* of these moved onto the
  instance in Slice 1; the registration *writes* wait for the flip.
- **Hub formatters** in `format.ts`: `describePosting` reads the event via
  `payment.eventInstance.event` / `payment.eventRegistration.event`; the
  `eventRegistration.event` path disappears with `event_registration`.
- **Trainer-based permissions** — `eventActions` / `eventInstanceActions` gate
  edit/export on `eventTrainersList.some(isMyPerson)` (`event_trainer`); these
  become `event_instance_trainer` / `can_trainer_edit_instance`.
- **The camp list itself** — `EventListDocument` is `events(condition: {type:
  CAMP})`. Camps become top-level `event_instance` (`type = camp`, `parent_id IS
  NULL`), so the sidebar `EventList` and the `/akce` index requery against
  instances.
- **`eventId`-based routing** — the per-instance page already moved to
  `/termin/[instance]` (301 from `/akce/[id]/termin/[instance]`); the `/akce/[id]`
  index and the two `export-event-*` reports still key on event id.

---

# Out of scope

Separate efforts; none block the refactor or the camp schedule.

- **Invitation vs registration** — calendar-visible-but-not-booked, declaring
  "not coming" to free a seat, and the fully-declarative (computed) cohort roster.
- **Cohort soft-hold vs hard-book** capacity semantics.
- **Camp roles** — admin / co-organizing trainer / guest trainer with differing
  edit rights. Until then, coarse parent→child trainer inheritance applies
  (`can_trainer_edit_instance` walks `parent_id`; no series-peer inheritance).
- **Non-member people** — external registrants (`event_external_registration`)
  should eventually become real `person` rows folded into the registration table.
  Blocked on the `tenant_*` mapping model (`tenant_membership` / `tenant_trainer`
  / `tenant_administrator`), which has no clean non-member representation. Interim:
  repoint `event_external_registration` to `instance_id`, keep it parallel.
- **Federated / WDSF** tables (`federated.competition`, `judge_score`,
  `event_official`) — a different meaning of "event".
