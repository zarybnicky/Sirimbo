import {
  createEventDefaults,
  type CreateEventDefaults,
  editEventDefaults,
  splitIntoLessonRanges,
} from '@/calendar/eventDefaults';
import { type EventWithTrainerFragment, SaveEventsDocument } from '@/graphql/Event';
import { useAuth, useTenantConfig } from '@/lib/auth';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatEventName, formatEventType, shortTimeFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { useSetAtom } from 'jotai';
import { eventTypes, formPreviewAtom } from '@/calendar/state';
import { Controller, useForm, useWatch } from 'react-hook-form';
import { useMutation } from 'urql';
import { EventOverlapWarning, useEventOverlaps } from './useEventOverlaps';
import { DateTimeRangeField } from './DateTimeRangeField';
import { InstanceListElement } from './InstanceListElement';
import { LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListElement';
import { useEventFormOptions } from './useEventFormOptions';
import { type EventFormInput, EventFormSchema, type EventFormType } from './types';

const eventTypeOptions = eventTypes.map((type) => ({
  id: type,
  label: formatEventType(type),
}));

type EventFormProps = CreateEventDefaults & {
  event?: EventWithTrainerFragment;
  seriesId?: string;
};

export function EventForm(input: EventFormProps) {
  const auth = useAuth();
  const { lockEventsByDefault } = useTenantConfig();
  const { onSuccess } = useFormResult();
  const [result, saveEvents] = useMutation(SaveEventsDocument);

  const { event } = input;
  const mode = event ? 'edit' : 'create';

  const { data, error, participants } = useEventFormOptions({
    eventId: event?.id,
    withRegistrations: !!event,
  });
  const ready = !!data;

  const parent = input.parent ?? data?.event?.parent;
  const parentId = input.parent?.id ?? event?.parentId ?? input.after?.parentId;
  const seriesId = input.seriesId ?? event?.seriesId;

  const trainerOptions = React.useMemo(
    () =>
      (data?.tenant?.tenantTrainersList ?? [])
        .filter(keyIsNonNull('person'))
        .map(({ person }) => ({
          id: person.id,
          label: person.name,
          hidden: !auth.isAdmin && !auth.isMyPerson(person.id),
          disabled:
            !!parent && !parent.eventTrainersList.some((x) => x.personId === person.id),
        })),
    [data?.tenant?.tenantTrainersList, auth, parent],
  );

  const [now] = React.useState(() => new Date());
  const defaultValues = React.useMemo(() => {
    if (event) return editEventDefaults(event, data, now.getTime());
    const enabledTrainers = trainerOptions.filter((x) => !x.hidden && !x.disabled);
    const trainer =
      (enabledTrainers.length === 1 ? enabledTrainers[0] : undefined) ??
      auth.persons.find(
        (p) => p.roles.includes('TRAINER') && enabledTrainers.some((x) => x.id === p.id),
      );

    return createEventDefaults(
      { ...input, date: input.date ?? now },
      {
        lockEventsByDefault,
        trainerId: trainer?.id,
        allowedTrainerIds: enabledTrainers.map((x) => x.id),
      },
    );
  }, [event, input, lockEventsByDefault, now, data, auth.persons, trainerOptions]);

  const { control, handleSubmit, setValue } = useForm<
    EventFormInput,
    unknown,
    EventFormType
  >({
    resolver: zodResolver(EventFormSchema),
    resetOptions: { keepDirtyValues: true },
    values: defaultValues,
  });
  const [
    type,
    instances,
    isPublic,
    trainers,
    registrations,
    locationId,
    locationText,
    name,
  ] = useWatch({
    control,
    name: [
      'type',
      'instances',
      'isPublic',
      'trainers',
      'registrations',
      'locationId',
      'locationText',
      'name',
    ],
  });
  const draft = React.useMemo<EventWithTrainerFragment>(
    () => ({
      id: event?.id ?? 'form-preview',
      name: name ?? '',
      type: type ?? 'LESSON',
      since: '',
      until: '',
      summary: null,
      capacity: 0,
      capacityUnit: 'REGISTRATIONS',
      shareToken: null,
      enableNotes: false,
      seriesId: null,
      parentId: parentId ?? null,
      managerPersonIds: (trainers ?? []).map((x) => x.personId),
      isVisible: true,
      isPublic: isPublic ?? false,
      hasPublicDetails: false,
      isCancelled: false,
      registrationPeriod: null,
      seriesInfo: null,
      registrationInfo: null,
      approxPriceList: null,
      parent: null,
      stats: { total: 0, unknown: 0, attended: 0, notExcused: 0 },
      location: data?.tenant?.locationsList?.find((x) => x.id === locationId) ?? null,
      locationText: locationId === 'other' ? (locationText ?? '') : '',
      targetCohortsList: [],
      eventTrainersList: (trainers ?? []).map(({ personId }) => ({
        id: personId,
        personId,
        lessonsOffered: 0,
        person: {
          id: personId,
          name: trainerOptions.find((x) => x.id === personId)?.label ?? '',
        },
      })),
      registrations: {
        nodes: (registrations ?? []).flatMap(({ personId, coupleId, isCancelled }) => {
          const participant = participants.get(
            personId ? `person:${personId}` : `couple:${coupleId}`,
          );
          return !isCancelled && participant
            ? {
                id: participant.id,
                person:
                  participant.person && personId
                    ? { ...participant.person, id: personId }
                    : null,
                couple:
                  participant.couple && coupleId
                    ? { ...participant.couple, id: coupleId }
                    : null,
              }
            : [];
        }),
      },
    }),
    [
      event?.id,
      name,
      type,
      parentId,
      trainers,
      isPublic,
      locationId,
      locationText,
      registrations,
      data,
      trainerOptions,
      participants,
    ],
  );

  const setPreview = useSetAtom(formPreviewAtom);
  React.useEffect(() => {
    const resourceKeys = (trainers ?? []).map((x) => `person:${x.personId}`);
    if (locationId === 'other') {
      if (locationText) resourceKeys.push(`locationText:${locationText}`);
    } else if (locationId && locationId !== 'none') {
      resourceKeys.push(`location:${locationId}`);
    }
    setPreview({
      events: instances
        .filter((x) => !x.isCancelled)
        .map((x, index) => ({
          kind: 'event',
          id: `form-preview-${index}`,
          __isPreview: true,
          start: new Date(x.since),
          end: new Date(x.until),
          resourceKeys,
          instance: { ...draft, since: x.since, until: x.until },
        })),
      resourceKeys: resourceKeys.length > 0 ? resourceKeys : ['unassigned'],
    });
    return () => setPreview(null);
  }, [draft, instances, trainers, locationId, locationText, setPreview]);

  React.useEffect(() => {
    if (!isPublic) setValue('hasPublicDetails', false);
  }, [isPublic, setValue]);

  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitIds, setSplitIds] = React.useState<Record<string, string | null>>({});
  const lessonRanges = instances[0]
    ? splitIntoLessonRanges(new Date(instances[0].since), new Date(instances[0].until))
    : [];
  const canSplit =
    mode === 'create' &&
    type === 'LESSON' &&
    lessonRanges.length >= 2 &&
    instances.length === 1;
  const isSplitting = canSplit && splitLessons;
  const trainerIds = (trainers ?? []).map((x) => x.personId);
  const participantIds = (registrations ?? [])
    .filter((x) => !x.isCancelled)
    .flatMap((x) =>
      x.personId
        ? [x.personId]
        : (participants.get(`couple:${x.coupleId}`)?.personIds ?? []),
    );
  const overlaps = useEventOverlaps(
    isSplitting
      ? lessonRanges.map(({ since, until }) => ({
          since: since.toISOString(),
          until: until.toISOString(),
          personIds: [
            ...trainerIds,
            ...(participants.get(splitIds[since.toISOString()] ?? '')?.personIds ?? []),
          ],
        }))
      : instances.map((x) => ({ ...x, personIds: [...trainerIds, ...participantIds] })),
    parentId,
  );

  React.useEffect(() => {
    if (!canSplit && splitLessons) {
      // eslint-disable-next-line react-hooks/set-state-in-effect -- Clear the state when lesson splitting becomes invalid.
      setSplitLessons(false);
    }
  }, [canSplit, splitLessons]);

  const onSubmit = async (values: EventFormType) => {
    if (!ready) return;
    const events = isSplitting
      ? lessonRanges.map(({ since, until }) => {
          const registration = participants.get(splitIds[since.toISOString()] ?? '');
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
          registrationPeriod: values.registrationPeriod,
          enableNotes: values.enableNotes,
        },
        events,
        trainers: values.trainers,
        cohortIds: values.cohortIds,
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
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error ?? error} />

      {canSplit && (
        <Checkbox
          name="splitLessons"
          checked={splitLessons}
          onChange={() => setSplitLessons((x) => !x)}
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
          <fieldset disabled={!ready} className="min-w-0">
            <LocationField
              control={control}
              locations={data?.tenant?.locationsList ?? []}
            />
            <TrainerListElement
              control={control}
              trainers={trainerOptions}
              conflicts={overlaps.byPerson}
            />
          </fieldset>
          <div className="grid gap-1 rounded-md border border-neutral-4 bg-neutral-2 p-2">
            {lessonRanges.map((range, index) => {
              const key = range.since.toISOString();
              return (
                <div
                  key={key}
                  className="flex flex-wrap items-center justify-between gap-2 text-sm"
                >
                  <span className="text-neutral-11">
                    {shortTimeFormatter.formatRange(range.since, range.until)}
                  </span>
                  <div className="flex items-center gap-2">
                    <EventOverlapWarning lines={overlaps.byInstance[index]} />
                    <ComboboxButton
                      disabled={!ready}
                      value={splitIds[key] ?? null}
                      options={[...participants.values()]}
                      placeholder="Volno"
                      buttonClassName={
                        splitIds[key]
                          ? undefined
                          : 'border-green-7 bg-green-3 text-green-11 hover:border-green-7 hover:bg-green-3/80'
                      }
                      onChange={(x) =>
                        setSplitIds((prev) => ({ ...prev, [key]: x ?? null }))
                      }
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
            options={
              event?.type === 'RESERVATION'
                ? [
                    ...eventTypeOptions,
                    { id: 'RESERVATION', label: formatEventType('RESERVATION') },
                  ]
                : eventTypeOptions
            }
          />
          <TextFieldElement
            control={control}
            name="name"
            label={'Název (nepovinný)'}
            placeholder={formatEventName({ ...draft, name: null }, 'neutral')}
          />
          <InstanceListElement control={control} conflicts={overlaps.byInstance} />
          <fieldset disabled={!ready} className="min-w-0 space-y-2">
            <LocationField
              control={control}
              locations={data?.tenant?.locationsList ?? []}
            />
            <TrainerListElement
              control={control}
              trainers={trainerOptions}
              conflicts={overlaps.byPerson}
            />
            <ParticipantListElement
              control={control}
              options={participants}
              conflicts={overlaps.byPerson}
              cohorts={data?.tenant?.cohortsList ?? []}
            />
          </fieldset>
        </>
      )}

      <div className="grid grid-cols-2 items-baseline justify-between gap-x-1">
        <CheckboxElement control={control} name="isVisible" label="Viditelná pro členy" />
        <Controller
          control={control}
          name="registrationPeriod"
          render={({ field }) => (
            <Checkbox
              name={field.name}
              label="Povolit přihlášky"
              checked={field.value !== null}
              onBlur={field.onBlur}
              onChange={() =>
                field.onChange(field.value !== null ? null : { start: null, end: null })
              }
            />
          )}
        />
        <CheckboxElement
          control={control}
          name="isPublic"
          label="Viditelná pro veřejnost"
        />
        <CheckboxElement
          control={control}
          name="hasPublicDetails"
          label="Veřejný program"
          disabled={!isPublic}
        />
        {type === 'CAMP' && (
          <CheckboxElement
            control={control}
            name="enableNotes"
            label="Poznámky k přihlášce"
          />
        )}
        {mode === 'edit' && (
          <CheckboxElement
            control={control}
            name="instances.0.isCancelled"
            label="Zrušeno"
          />
        )}
      </div>

      <div className="flex justify-end pt-1">
        <SubmitButton control={control} disabled={!ready}>
          {mode === 'create' ? 'Vytvořit' : 'Uložit'}
        </SubmitButton>
      </div>
    </form>
  );
}
