import {
  createEventDefaults,
  editEventDefaults,
  splitIntoLessonRanges,
  type CreateEventDefaults,
} from '@/calendar/eventDefaults';
import {
  SaveEventsDocument,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
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
import { formPreviewAtom } from '@/calendar/state';
import { Controller, useForm, useWatch } from 'react-hook-form';
import { useMutation } from 'urql';
import { DateTimeRangeField } from './DateTimeRangeField';
import { InstanceListElement } from './InstanceListElement';
import { LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListElement';
import { useEventFormOptions } from './useEventFormOptions';
import {
  EventFormSchema,
  type EventFormInput,
  type EventFormType,
} from './types';

const eventTypeOptions = (
  ['LESSON', 'GROUP', 'RESERVATION', 'CAMP', 'HOLIDAY'] as const
).map((type) => ({ id: type, label: formatEventType(type) }));

type EventFormProps = CreateEventDefaults & {
  event?: EventWithTrainerFragment;
  seriesId?: string;
};

function useEventFormSetup(input: EventFormProps) {
  const { event } = input;
  const auth = useAuth();
  const { lockEventsByDefault } = useTenantConfig();
  const [now] = React.useState(() => new Date());
  const query = useEventFormOptions({
    eventId: event?.id, withRegistrations: !!event,
  });
  const parent = input.parent ?? query.data?.event?.parent;
  const trainers = React.useMemo(() => (query.data?.tenant?.tenantTrainersList ?? [])
    .filter(keyIsNonNull('person'))
    .map(({ person }) => ({
      id: person.id,
      label: person.name,
      hidden: !auth.isAdmin && !auth.isMyPerson(person.id),
      disabled: !!parent && !parent.eventTrainersList.some((x) => x.personId === person.id),
    })), [query.data?.tenant?.tenantTrainersList, auth, parent]);

  const defaultValues = React.useMemo(() => {
    if (event) return editEventDefaults(event, query.data, now.getTime());
    const enabledTrainers = trainers.filter((x) => !x.hidden && !x.disabled);
    const trainer =
      (enabledTrainers.length === 1 ? enabledTrainers[0] : undefined) ??
        auth.persons.find((p) => p.isTrainer && enabledTrainers.some((x) => x.id === p.id));

    return createEventDefaults(
      { ...input, date: input.date ?? now },
      {
        lockEventsByDefault,
        trainerId: trainer?.id,
        allowedTrainerIds: enabledTrainers.map((x) => x.id),
      },
    );
  }, [event, input, lockEventsByDefault, now, query.data, auth.persons, trainers]);

  return {
    ...query,
    trainers,
    defaultValues,
  };
}

export function EventForm(input: EventFormProps) {
  const { event } = input;
  const setPreview = useSetAtom(formPreviewAtom);
  const options = useEventFormSetup(input);
  const { defaultValues } = options;
  const mode = event ? 'edit' : 'create';
  const parentId = input.parent?.id ?? event?.parentId ?? input.after?.parentId;
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
  const [type, instances, isPublic, trainers, registrations, locationId, locationText, name] = useWatch({
    control,
    name: ['type', 'instances', 'isPublic', 'trainers', 'registrations', 'locationId', 'locationText', 'name'],
  });
  const draft = React.useMemo<EventWithTrainerFragment>(() => ({
    id: event?.id ?? 'form-preview',
    name: name ?? '',
    type: type ?? 'LESSON',
    since: '', until: '',
    summary: null, capacity: 0, capacityUnit: 'REGISTRATIONS',
    shareToken: null, enableNotes: false, seriesId: null, parentId: parentId ?? null,
    managerPersonIds: (trainers ?? []).map((x) => x.personId),
    isVisible: true, isPublic: isPublic ?? false, hasPublicDetails: false,
    isCancelled: false, registrationPeriod: null, seriesInfo: null,
    registrationInfo: null, approxPriceList: null, parent: null,
    stats: { total: 0, unknown: 0, attended: 0, notExcused: 0 },
    location: options.data?.tenant?.locationsList?.find((x) => x.id === locationId) ?? null,
    locationText: locationId === 'other' ? locationText ?? '' : '',
    targetCohortsList: [],
    eventTrainersList: (trainers ?? []).map(({ personId }) => ({
      id: personId, personId, lessonsOffered: 0,
      person: { id: personId, name: options.trainers.find((x) => x.id === personId)?.label ?? '' },
    })),
    registrations: {
      nodes: (registrations ?? []).flatMap(({ personId, coupleId, isCancelled }) => {
        const participant = options.participants.get(personId ? `person:${personId}` : `couple:${coupleId}`);
        return !isCancelled && participant ? [{
          id: participant.id,
          person: participant.person && personId ? { ...participant.person, id: personId } : null,
          couple: participant.couple && coupleId ? { ...participant.couple, id: coupleId } : null,
        }] : [];
      }),
    },
  }), [event?.id, name, type, parentId, trainers, isPublic, locationId, locationText, registrations, options.data, options.trainers, options.participants]);
  const namePlaceholder = formatEventName({ ...draft, name: null }, 'neutral');

  React.useEffect(() => {
    const resourceKeys = (trainers ?? []).map((x) => `person:${x.personId}`);
    if (locationId === 'other') {
      if (locationText) resourceKeys.push(`locationText:${locationText}`);
    } else if (locationId && locationId !== 'none') {
      resourceKeys.push(`location:${locationId}`);
    }
    setPreview({
      events: instances.filter((x) => !x.isCancelled).map((x, index) => ({
        kind: 'event', id: `form-preview-${index}`, __isPreview: true,
        start: new Date(x.since), end: new Date(x.until), resourceKeys,
        instance: { ...draft, since: x.since, until: x.until },
      })),
      resourceKeys: resourceKeys.length > 0 ? resourceKeys : ['unassigned'],
    });
    return () => setPreview(null);
  }, [draft, instances, trainers, locationId, locationText, setPreview]);

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
            <ParticipantListElement
              control={control}
              setValue={setValue}
              options={options.participants}
              cohorts={options.data?.tenant?.cohortsList ?? []}
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
