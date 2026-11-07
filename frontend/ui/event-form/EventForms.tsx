import {
  createEventDefaults,
  editEventDefaults,
  splitIntoLessonRanges,
  type CreateEventDefaults,
} from '@/calendar/eventDefaults';
import {
  EventFormOptionsDocument,
  SaveEventsDocument,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
import { useAuth, useTenantConfig } from '@/lib/auth';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatEventName, formatEventType, shortTimeFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { Controller, FormProvider, useForm, useWatch } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { DateTimeRangeField } from './DateTimeRangeField';
import { InstanceListElement } from './InstanceListElement';
import { LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListElement';
import {
  EventForm as EventFormSchema,
  type EventFormInput,
  type EventFormType,
  type EventParticipantOption,
} from './types';

const eventTypeOptions = (
  ['LESSON', 'GROUP', 'RESERVATION', 'CAMP', 'HOLIDAY'] as const
).map((type) => ({ id: type, label: formatEventType(type) }));

type EventFormProps = CreateEventDefaults & {
  event?: EventWithTrainerFragment;
  parentId?: string;
  seriesId?: string;
};

function useEventFormOptions(input: EventFormProps) {
  const { event } = input;
  const auth = useAuth();
  const { lockEventsByDefault } = useTenantConfig();
  const [now] = React.useState(() => new Date());
  const [query] = useQuery({
    query: EventFormOptionsDocument,
    variables: { eventId: event?.id, withRegistrations: !!event },
  });
  const savedRegistrations = query.data?.registrations;
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
        label: person.name + (person.isExternal ? ' (Externí)' : ''),
        isExternal: person.isExternal,
        personId: person.id,
        coupleId: null,
        personIds: [person.id],
        person,
        couple: null,
      });
    }
    for (const couple of [
      ...(savedRegistrations ?? []).flatMap((x) => x.couple || []),
      ...(query.data?.couplesList ?? []),
    ]) {
      participants.set(`couple:${couple.id}`, {
        id: `couple:${couple.id}`,
        label: couple.name,
        isExternal: ![couple.manId, couple.womanId].some(
          (id) => !participants.get(`person:${id}`)?.isExternal,
        ),
        personId: null,
        coupleId: couple.id,
        personIds: [couple.manId, couple.womanId],
        person: null,
        couple,
      });
    }
    return new Map([...participants].toSorted(([, a], [, b]) =>
      Number(a.isExternal) - Number(b.isExternal) ||
      compareText(a.label, b.label),
    ));
  }, [query.data?.couplesList, query.data?.tenant, savedRegistrations]);

  const enabledTrainers = trainers.filter((x) => x.enabled);
  const trainer =
    (enabledTrainers.length === 1 ? enabledTrainers[0] : undefined) ??
      auth.persons.find((p) => p.isTrainer && enabledTrainers.some((x) => x.id === p.id));

  const defaultValues = React.useMemo(
    () => event
      ? editEventDefaults(event, query.data, now.getTime())
      : {
          ...createEventDefaults({ ...input, date: input.date ?? now }, trainer?.id),
          registrationPeriod: lockEventsByDefault ? null : { start: null, end: null },
        },
    [event, input, lockEventsByDefault, now, query.data, trainer?.id],
  );

  return {
    ...query,
    trainers,
    participants,
    defaultValues,
  };
}

export function EventForm(input: EventFormProps) {
  const { event } = input;
  const options = useEventFormOptions(input);
  const { defaultValues } = options;
  const mode = event ? 'edit' : 'create';
  const parentId = input.parentId ?? event?.parentId ?? input.after?.parentId;
  const seriesId = input.seriesId ?? event?.seriesId;
  const ready = !!options.data;
  const { onSuccess } = useFormResult();
  const [result, saveEvents] = useMutation(SaveEventsDocument);
  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitIds, setSplitIds] = React.useState<Record<string, string | null>>({});
  const form = useForm<EventFormInput, unknown, EventFormType>({
    resolver: zodResolver(EventFormSchema),
    resetOptions: { keepDirtyValues: true },
    values: defaultValues,
  });
  const { control, handleSubmit, setValue } = form;
  const [type, instances, isPublic, trainers, registrations] = useWatch({
    control,
    name: ['type', 'instances', 'isPublic', 'trainers', 'registrations'],
  });
  const namePlaceholder = formatEventName({
    name: null,
    type: type ?? 'LESSON',
    eventTrainersList: (trainers ?? []).map(({ personId }) => ({
      person: { name: options.trainers.find((x) => x.id === personId)?.label },
    })),
    registrations: {
      nodes: (registrations ?? []).flatMap(({ personId, coupleId, isCancelled }) => {
        const participant = options.participants.get(
          personId ? `person:${personId}` : `couple:${coupleId}`,
        );
        return !isCancelled && participant ? [participant] : [];
      }),
    },
  }, 'neutral');

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
    if (!ready) return;
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
          registrationPeriod: values.registrationPeriod,
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
                locations={options.data?.tenant?.locationsList ?? []}
              />
              <TrainerListElement control={control} trainers={options.trainers} />
            </fieldset>
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
                        disabled={!ready}
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
              options={eventTypeOptions.filter(
                (x) => x.id !== 'RESERVATION' || defaultValues.type === 'RESERVATION',
              )}
            />
            <TextFieldElement
              control={control}
              name="name"
              label={mode === 'create' ? 'Název (nepovinný)' : 'Název'}
              placeholder={namePlaceholder}
            />
            <InstanceListElement control={control} />
            <fieldset disabled={!ready} className="min-w-0 space-y-2">
              <LocationField
                control={control}
                locations={options.data?.tenant?.locationsList ?? []}
              />
              <TrainerListElement control={control} trainers={options.trainers} />
            </fieldset>
            <fieldset disabled={!ready} className="min-w-0 space-y-2">
              <ParticipantListElement
                options={options.participants}
                cohorts={options.data?.tenant?.cohortsList ?? []}
              />
            </fieldset>
          </>
        )}

        <div className="grid grid-cols-2 items-baseline justify-between gap-x-1">
          <CheckboxElement name="isVisible" label="Viditelná pro členy" />
          <Controller
            control={control}
            name="registrationPeriod"
            render={({ field }) => (
              <Checkbox
                name={field.name}
                label="Povolit přihlášky"
                checked={field.value !== null}
                onBlur={field.onBlur}
                onChange={() => field.onChange(
                  field.value !== null ? null : { start: null, end: null },
                )}
              />
            )}
          />
          <CheckboxElement name="isPublic" label="Viditelná pro veřejnost" />
          <CheckboxElement name="hasPublicDetails" label="Veřejný program" disabled={!isPublic} />
          {type === 'CAMP' && (
            <CheckboxElement name="enableNotes" label="Poznámky k přihlášce" />
          )}
          {mode === 'edit' && (
            <CheckboxElement name="instances.0.isCancelled" label="Zrušeno" />
          )}
        </div>

        <div className="flex justify-end pt-1">
          <SubmitButton control={control} disabled={!ready}>
            {mode === 'create' ? 'Vytvořit' : 'Uložit'}
          </SubmitButton>
        </div>
      </form>
    </FormProvider>
  );
}
