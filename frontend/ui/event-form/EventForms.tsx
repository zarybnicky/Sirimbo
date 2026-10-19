import {
  createEventDefaults,
  splitIntoLessonRanges,
  type CreateEventDefaults,
} from '@/calendar/eventDefaults';
import {
  EventEditorRegistrationsDocument,
  EventFormOptionsDocument,
  SaveEventsDocument,
  type EventInstanceRegistrationFragment,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
import { useAuth, useTenantConfig } from '@/lib/auth';
import { isTruthy, keyIsNonNull } from '@/lib/truthyFilter';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatCoupleName, formatEventType, shortTimeFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { FormProvider, useForm, useWatch } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { DateTimeRangeField } from './DateTimeRangeField';
import { InstanceListElement } from './InstanceListElement';
import { LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListElement';
import {
  EventForm,
  type EventFormInput,
  type EventFormType,
  type EventParticipantOption,
} from './types';

const eventTypeOptions = (
  ['LESSON', 'GROUP', 'RESERVATION', 'CAMP', 'HOLIDAY'] as const
).map((type) => ({ id: type, label: formatEventType(type) }));

function useEventFormOptions(savedRegistrations?: EventInstanceRegistrationFragment[]) {
  const auth = useAuth();
  const [query] = useQuery({ query: EventFormOptionsDocument });
  const trainers = (query.data?.tenant?.tenantTrainersList ?? [])
    .filter(keyIsNonNull('person'))
    .map(({ person }) => ({
      id: person.id,
      label: person.name,
      enabled: auth.isAdmin || auth.isMyPerson(person.id),
    }));

  const participants = React.useMemo(() => {
    const tenant = query.data?.tenant;
    const participants = new Map<string, EventParticipantOption>();

    for (const { person } of [
      ...(savedRegistrations ?? []),
      ...(tenant?.cohortsList ?? []).flatMap((x) => x.cohortMembershipsList),
      ...(tenant?.tenantMembershipsList ?? []),
    ]) {
      if (!person) continue;
      participants.set(`person:${person.id}`, {
        id: `person:${person.id}`,
        label: person.name,
        personId: person.id,
        coupleId: null,
        personIds: [person.id],
      });
    }
    for (const couple of [
      ...(savedRegistrations ?? []).map((x) => x.couple),
      ...(tenant?.couplesList ?? []).filter((x) => x.status === 'ACTIVE'),
    ]) {
      if (!couple) continue;
      participants.set(`couple:${couple.id}`, {
        id: `couple:${couple.id}`,
        label: formatCoupleName(couple),
        personId: null,
        coupleId: couple.id,
        personIds: [couple.man?.id, couple.woman?.id].filter(isTruthy),
      });
    }

    return participants;
  }, [query.data?.tenant, savedRegistrations]);
  return { ...query, trainers, participants };
}

function EventEditor({
  defaultValues,
  parentId,
  seriesId,
  options,
  mode,
}: {
  defaultValues: EventFormType;
  parentId?: string | null;
  seriesId?: string | null;
  options: ReturnType<typeof useEventFormOptions>;
  mode: 'create' | 'edit';
}) {
  const { onSuccess } = useFormResult();
  const [result, saveEvents] = useMutation(SaveEventsDocument);
  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitIds, setSplitIds] = React.useState<Record<string, string | null>>({});
  const form = useForm<EventFormInput, unknown, EventFormType>({
    resolver: zodResolver(EventForm),
    defaultValues,
  });
  const { control, handleSubmit, setValue } = form;
  const [type, instances, isPublic] = useWatch({ control, name: ['type', 'instances', 'isPublic'] });

  React.useEffect(() => {
    if (!isPublic) setValue('hasPublicDetails', false);
  }, [isPublic, setValue]);

  const first = instances[0];
  const lessonRanges = first
    ? splitIntoLessonRanges(new Date(first.since), new Date(first.until))
    : [];
  const canSplit =
    mode === 'create' &&
    type === 'LESSON' &&
    lessonRanges.length >= 2 &&
    instances.length === 1;
  const isSplitting = canSplit && splitLessons;

  React.useEffect(() => {
    if (!canSplit && splitLessons) {
      // eslint-disable-next-line react-hooks/set-state-in-effect -- Clear the state when lesson splitting becomes invalid.
      setSplitLessons(false);
    }
  }, [canSplit, splitLessons]);

  const onSubmit = async (values: EventFormType) => {
    const events = isSplitting
      ? lessonRanges.map(({ since, until }) => {
          const registration = options.participants.get(
            splitIds[since.toISOString()] ?? '',
          );
          return {
            since: since.toISOString(),
            until: until.toISOString(),
            registrations: registration
              ? [
                  {
                    personId: registration.personId,
                    coupleId: registration.coupleId,
                  },
                ]
              : [],
          };
        })
      : values.instances.map(({ itemId: id, since, until, isCancelled }) => ({
          id,
          since,
          until,
          isCancelled,
          registrations: values.registrations.map(({ cohortIds, ...reg }) => ({
            ...reg,
            targetCohortId: cohortIds[0] ?? null,
          })),
        }));

    const result = await saveEvents({
      input: {
        details: {
          parentId,
          name: isSplitting ? null : values.name.trim() || null,
          type: values.type,
          locationId: !['none', 'other'].includes(values.locationId)
            ? values.locationId
            : null,
          locationText: values.locationId === 'none' ? '' : values.locationText,
          capacity: mode === 'edit' ? values.capacity : values.type === 'LESSON' ? 1 : 0,
          capacityUnit:
            mode === 'edit'
              ? values.capacityUnit
              : values.type === 'LESSON'
                ? 'REGISTRATIONS'
                : 'PEOPLE',
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          hasPublicDetails: values.hasPublicDetails,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
        },
        events,
        trainers: values.trainers,
        cohortIds: values.cohorts.map(({ cohortId }) => cohortId),
        series:
          !isSplitting && events.length > 1
            ? seriesId
              ? { id: seriesId }
              : { name: values.name.trim() }
            : null,
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <FormProvider {...form}>
      <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
        <FormError error={result.error ?? options.error} />

        {canSplit && (
          <Checkbox
            name="splitLessons"
            checked={splitLessons}
            onChange={() => setSplitLessons((value) => !value)}
            label={`Rozdělit na samostatné lekce? (${lessonRanges.length} x 45 min)`}
          />
        )}

        {isSplitting ? (
          <>
            <DateTimeRangeField
              control={control}
              nameSince="instances.0.since"
              nameUntil="instances.0.until"
            />
            <TrainerListElement control={control} trainers={options.trainers} />
            <LocationField
              control={control}
              locations={options.data?.tenant?.locationsList ?? []}
            />
            <div className="grid gap-1 rounded-md border border-neutral-4 bg-neutral-2 p-2">
              {lessonRanges.map((range) => {
                const key = range.since.toISOString();
                return (
                  <div
                    key={key}
                    className="flex flex-wrap items-center justify-between gap-2 text-sm"
                  >
                    <span className="text-neutral-11">
                      {shortTimeFormatter.formatRange(range.since, range.until)}
                    </span>
                    <div className="sm:justify-self-end">
                      <ComboboxButton
                        value={splitIds[key] ?? null}
                        options={[...options.participants.values()]}
                        placeholder="Volno"
                        buttonClassName={splitIds[key] ? undefined : 'border-green-7 bg-green-3 text-green-11 hover:border-green-7 hover:bg-green-3/80'}
                        onChange={(x) => setSplitIds((prev) => ({ ...prev, [key]: x ?? null }))}
                      />
                    </div>
                  </div>
                );
              })}
            </div>
          </>
        ) : (
          <>
            <RadioButtonGroupElement
              control={control}
              name="type"
              options={eventTypeOptions}
            />
            <TextFieldElement
              control={control}
              name="name"
              label={mode === 'create' ? 'Název (nepovinný)' : 'Název'}
            />
            <InstanceListElement control={control} />
            <LocationField
              control={control}
              locations={options.data?.tenant?.locationsList ?? []}
            />
            <TrainerListElement control={control} trainers={options.trainers} />
            <ParticipantListElement
              options={options.participants}
              cohorts={options.data?.tenant?.cohortsList ?? []}
            />
          </>
        )}

        <div className="flex flex-wrap items-baseline justify-between gap-x-1">
          <CheckboxElement name="isVisible" label="Viditelná pro členy" />
          <CheckboxElement name="isLocked" label="Zakázat přihlašování/odhlašování" />
          <CheckboxElement name="isPublic" label="Zveřejnit přihlášky" />
          <CheckboxElement name="hasPublicDetails" label="Zveřejnit program" disabled={!isPublic} />
          {(type === 'RESERVATION' || type === 'CAMP') && (
            <CheckboxElement name="enableNotes" label="Povolit poznámky k přihlášce" />
          )}
        </div>
        {mode === 'edit' && (
          <CheckboxElement name="instances.0.isCancelled" label="Zrušeno" />
        )}

        <div className="flex justify-end pt-1">
          <SubmitButton control={control}>
            {mode === 'create' ? 'Vytvořit' : 'Uložit'}
          </SubmitButton>
        </div>
      </form>
    </FormProvider>
  );
}

export function CreateEventForm({
  parentId,
  ...input
}: CreateEventDefaults & { parentId?: string }) {
  const auth = useAuth();
  const { lockEventsByDefault } = useTenantConfig();
  const options = useEventFormOptions();
  if (!options.data)
    return options.error ? <FormError error={options.error} /> : <div>Načítám…</div>;

  const enabledTrainers = options.trainers.filter((x) => x.enabled);
  const trainer =
    auth.persons.find((p) => p.isTrainer && enabledTrainers.some((x) => x.id === p.id)) ??
    (enabledTrainers.length === 1 ? enabledTrainers[0] : undefined);

  return (
    <EventEditor
      mode="create"
      parentId={parentId ?? input.after?.parentId}
      options={options}
      defaultValues={createEventDefaults(input, {
        trainerPersonIds: trainer ? [trainer.id] : [],
        isLocked: lockEventsByDefault,
      })}
    />
  );
}

export function EditEventForm({ event }: { event: EventWithTrainerFragment }) {
  const [now] = React.useState(() => Date.now());
  const [query] = useQuery({
    query: EventEditorRegistrationsDocument,
    variables: { id: event.id },
    requestPolicy: 'network-only',
  });

  const registrations = query.data?.event?.registrationsList;
  const options = useEventFormOptions(registrations);
  if ((!registrations && query.fetching) || (!options.data && options.fetching)) {
    return <div className="text-sm text-neutral-11">Načítám účastníky…</div>;
  }

  if (!registrations || !options.data) {
    return query.error || options.error ? (
      <FormError error={query.error ?? options.error} />
    ) : (
      <div className="text-sm text-neutral-11">Událost není dostupná.</div>
    );
  }

  const at = Math.min(now, Date.parse(event.since));

  return (
    <EventEditor
      key={event.id}
      mode="edit"
      parentId={event.parentId}
      seriesId={event.seriesId}
      options={options}
      defaultValues={{
        name: event.name ?? '',
        type: event.type ?? 'LESSON',
        locationId: event.locationText ? 'other' : (event.location?.id ?? 'none'),
        locationText: event.locationText ?? '',
        capacity: event.capacity ?? 0,
        capacityUnit: event.capacityUnit,
        isVisible: event.isVisible ?? false,
        isPublic: event.isPublic ?? false,
        hasPublicDetails: event.hasPublicDetails,
        isLocked: event.isLocked ?? false,
        enableNotes: event.enableNotes ?? false,
        instances: [
          {
            itemId: event.id,
            since: event.since,
            until: event.until,
            isCancelled: event.isCancelled,
          },
        ],
        trainers: event.trainersList.map((trainer) => ({
          personId: trainer.personId,
          lessonsOffered: trainer.lessonsOffered,
        })),
        cohorts: event.targetCohortsList.map(({ cohortId }) => ({ cohortId })),
        registrations: registrations.map((r) => ({
          personId: r.personId,
          coupleId: r.coupleId,
          isCancelled: false,
          cohortIds: r.source !== 'COHORT' ? [] : event.targetCohortsList
            .filter(({ cohortId }) => cohortId === r.targetCohortId ||
              options.data?.tenant?.cohortsList.find((x) => x.id === cohortId)?.cohortMembershipsList
                .some((m) => m.person?.id === r.personId &&
                  (at < now || m.status === 'ACTIVE') && Date.parse(m.since) <= at &&
                  (!m.until || Date.parse(m.until) > at)))
            .map((x) => x.cohortId),
        })),
      }}
    />
  );
}
