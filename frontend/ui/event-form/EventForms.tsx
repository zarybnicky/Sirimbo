import {
  splitIntoLessonRanges,
  type CreateEventDefaults,
} from '@/calendar/eventDefaults';
import {
  EventRegistrationsDocument,
  SaveEventsDocument,
  type EventInstanceRegistrationFragment,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { useTenantConfig } from '@/lib/auth';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatCoupleName, formatEventType, shortTimeFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { FormProvider, useForm, useFormContext, useWatch } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { CohortListElement } from './CohortListElement';
import { DateTimeRangeField } from './DateTimeRangeField';
import { InstanceListElement } from './InstanceListElement';
import { LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListElement';
import { EventForm, type EventFormInput, type EventFormType } from './types';

const eventTypeOptions = (
  ['LESSON', 'GROUP', 'RESERVATION', 'CAMP', 'HOLIDAY'] as const
).map((type) => ({ id: type, label: formatEventType(type) }));

type Option = { id: string; label: string };

const SplitRegistrationPicker = React.memo(function SplitRegistrationPicker({
  rangeKey,
  value,
  options,
  setSplitIds,
}: {
  rangeKey: string;
  value: string | null;
  options: Option[];
  setSplitIds: React.Dispatch<React.SetStateAction<Record<string, string | null>>>;
}) {
  return (
    <ComboboxButton
      value={value}
      options={options}
      placeholder="Volno"
      buttonClassName={
        value
          ? undefined
          : 'border-green-7 bg-green-3 text-green-11 hover:border-green-7 hover:bg-green-3/80'
      }
      onChange={(selected) =>
        setSplitIds((current) => ({ ...current, [rangeKey]: selected ?? null }))
      }
    />
  );
});

function EventAccessFields() {
  const { control, setValue } = useFormContext<EventFormInput, unknown, EventFormType>();
  const [type, isPublic] = useWatch({ control, name: ['type', 'isPublic'] });

  React.useEffect(() => {
    if (!isPublic) setValue('hasPublicDetails', false);
  }, [isPublic, setValue]);

  return (
    <div className="flex flex-wrap items-baseline justify-between gap-x-1">
      <CheckboxElement name="isVisible" label="Viditelná pro členy" />
      <CheckboxElement name="isLocked" label="Zakázat přihlašování/odhlašování" />
      <CheckboxElement name="isPublic" label="Zveřejnit přihlášky" />
      <CheckboxElement
        name="hasPublicDetails"
        label="Zveřejnit program"
        disabled={!isPublic}
      />
      {(type === 'RESERVATION' || type === 'CAMP') && (
        <CheckboxElement name="enableNotes" label="Povolit poznámky k přihlášce" />
      )}
    </div>
  );
}

function EventEditor({
  defaultValues,
  parentId,
  seriesId,
  existingCohorts,
  existingRegistrations = [],
  mode,
}: {
  defaultValues: EventFormType;
  parentId?: string | null;
  seriesId?: string | null;
  existingCohorts?: Option[];
  existingRegistrations?: EventInstanceRegistrationFragment[];
  mode: 'create' | 'edit';
}) {
  const { onSuccess } = useFormResult();
  const [result, saveEvents] = useMutation(SaveEventsDocument);
  const [{ data: tenant }] = useQuery({
    query: CurrentTenantDocument,
    pause: mode === 'edit',
  });
  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitIds, setSplitIds] = React.useState<Record<string, string | null>>({});
  const form = useForm<EventFormInput, unknown, EventFormType>({
    resolver: zodResolver(EventForm),
    defaultValues,
  });
  const { control, handleSubmit } = form;
  const [type, instances] = useWatch({ control, name: ['type', 'instances'] });
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
  const splitRegistrationOptions = React.useMemo(
    () => [
      ...(tenant?.tenant?.couplesList ?? []).flatMap((couple) =>
        couple.status === 'ACTIVE'
          ? [{ id: `couple:${couple.id}`, label: formatCoupleName(couple) }]
          : [],
      ),
      ...(tenant?.tenant?.tenantMembershipsList ?? []).flatMap((membership) =>
        membership.status === 'ACTIVE' && membership.person?.id
          ? [{ id: `person:${membership.person.id}`, label: membership.person.name }]
          : [],
      ),
    ],
    [tenant],
  );

  React.useEffect(() => {
    if (!canSplit && splitLessons) setSplitLessons(false);
  }, [canSplit, splitLessons]);

  const onSubmit = async (values: EventFormType) => {
    const events = isSplitting
      ? lessonRanges.map(({ since, until }) => {
          const sinceString = since.toISOString();
          const [registrationType, registrationId] =
            splitIds[sinceString]?.split(':') ?? [];
          return {
            since: sinceString,
            until: until.toISOString(),
            registrations: registrationId
              ? [
                  {
                    personId: registrationType === 'person' ? registrationId : null,
                    coupleId: registrationType === 'couple' ? registrationId : null,
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
          registrations: values.registrations,
        }));
    const name = values.name.trim();
    const isLesson = values.type === 'LESSON';
    const result = await saveEvents({
      input: {
        details: {
          parentId,
          name: isSplitting ? null : name || null,
          type: values.type,
          locationId: !['none', 'other'].includes(values.locationId)
            ? values.locationId
            : null,
          locationText: values.locationId === 'none' ? '' : values.locationText,
          capacity: mode === 'edit' ? values.capacity : isLesson ? 1 : 0,
          capacityUnit:
            mode === 'edit' ? values.capacityUnit : isLesson ? 'REGISTRATIONS' : 'PEOPLE',
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
              : { name }
            : null,
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <FormProvider {...form}>
      <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
        <FormError error={result.error} />

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
            <TrainerListElement control={control} mode={mode} />
            <LocationField control={control} />
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
                      <SplitRegistrationPicker
                        rangeKey={key}
                        value={splitIds[key] ?? null}
                        options={splitRegistrationOptions}
                        setSplitIds={setSplitIds}
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
            <LocationField control={control} />
            <TrainerListElement control={control} mode={mode} />
            <CohortListElement control={control} existingCohorts={existingCohorts} />
            <ParticipantListElement
              control={control}
              existingPeople={existingRegistrations.flatMap(({ person }) =>
                person ? [{ id: person.id, label: person.name }] : [],
              )}
              existingCouples={existingRegistrations.flatMap(({ couple }) =>
                couple ? [{ id: couple.id, label: formatCoupleName(couple) }] : [],
              )}
            />
          </>
        )}

        <EventAccessFields />
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
  defaults,
  parentId,
}: {
  defaults: CreateEventDefaults;
  parentId?: string;
}) {
  const { lockEventsByDefault } = useTenantConfig();
  const type = defaults.type ?? 'LESSON';

  return (
    <EventEditor
      mode="create"
      parentId={parentId}
      defaultValues={{
        name: '',
        type,
        locationId: defaults.locationText ? 'other' : (defaults.locationId ?? 'none'),
        locationText: defaults.locationText,
        capacity: type === 'LESSON' ? 1 : 0,
        capacityUnit: type === 'LESSON' ? 'REGISTRATIONS' : 'PEOPLE',
        isVisible: true,
        isPublic: false,
        hasPublicDetails: false,
        isLocked: lockEventsByDefault,
        enableNotes: false,
        instances: [
          {
            itemId: null,
            since: defaults.since.toISOString(),
            until: defaults.until.toISOString(),
            isCancelled: false,
          },
        ],
        trainers: defaults.trainerPersonIds.map((personId) => ({
          personId,
          lessonsOffered: 0,
        })),
        cohorts: [],
        registrations: [],
      }}
    />
  );
}

export function EditEventForm({ event }: { event: EventWithTrainerFragment }) {
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id: event.id },
    requestPolicy: 'network-only',
  });
  const registrationsEvent = query.data?.event;
  const registrations =
    registrationsEvent?.id === event.id
      ? registrationsEvent.registrationsList
      : undefined;

  if (!registrations && query.fetching) {
    return <div className="text-sm text-neutral-11">Načítám účastníky…</div>;
  }

  if (!registrations) {
    return query.error ? (
      <FormError error={query.error} />
    ) : (
      <div className="text-sm text-neutral-11">Událost není dostupná.</div>
    );
  }

  return (
    <EventEditor
      key={event.id}
      mode="edit"
      parentId={event.parentId}
      seriesId={event.seriesId}
      existingRegistrations={registrations}
      existingCohorts={event.targetCohortsList.map((target) => ({
        id: target.cohortId,
        label: target.cohort?.name ?? '-',
      }))}
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
        registrations: registrations.map((registration) => ({
          personId: registration.personId,
          coupleId: registration.coupleId,
        })),
      }}
    />
  );
}
