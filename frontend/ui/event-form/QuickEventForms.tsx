import type { EventType } from '@/graphql';
import {
  type EventInstanceWithTrainerFragment,
  EventInstanceRegistrationsDocument,
  CreateEventInstancesDocument,
  UpdateEventInstanceDetailsDocument,
} from '@/graphql/Event';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import {
  splitIntoLessonRanges,
  type QuickEventCreateDefaults,
} from '@/calendar/quickEventDefaults';
import { Checkbox, CheckboxElement } from '@/ui/fields/checkbox';
import {
  RadioButtonGroupElement,
  type RadioButtonGroupItem,
} from '@/ui/fields/RadioButtonGroupElement';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { formatEventType, formatLongCoupleName, shortTimeFormatter } from '@/ui/format';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { type Control, useForm, useWatch } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from '@/ui/state/auth';
import { DateTimeRangeController, InstanceListElement } from './InstanceListElement';
import { InstanceTrainerListElement } from './InstanceTrainerListField';
import { eventLocationInput, LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListField';
import { CohortListElement } from './CohortListElement';
import { EventForm, type EventFormType } from './types';

const eventTypeOptions: RadioButtonGroupItem[] = [
  'LESSON',
  'GROUP',
  'RESERVATION',
  'CAMP',
  'HOLIDAY',
].map((type) => ({ id: type, label: formatEventType(type as EventType) }));

type EventFormInput = z.input<typeof EventForm>;

function EventAccessFields({
  control,
  type,
}: {
  control: Control<EventFormInput, unknown, EventFormType>;
  type: EventFormType['type'];
}) {
  return (
    <div className="flex flex-wrap items-baseline justify-between gap-x-1">
      <CheckboxElement control={control} name="isVisible" label="Viditelná pro členy" />
      <CheckboxElement
        control={control}
        name="isLocked"
        label="Zakázat přihlašování/odhlašování"
      />
      <CheckboxElement control={control} name="isPublic" label="Viditelná pro veřejnost" />
      {(type === 'RESERVATION' || type === 'CAMP') && (
        <CheckboxElement
          control={control}
          name="enableNotes"
          label="Povolit poznámky k přihlášce"
        />
      )}
    </div>
  );
}

export function QuickEventCreateForm({
  defaults,
  parentId,
  initialType = 'LESSON',
}: {
  defaults: QuickEventCreateDefaults;
  parentId?: string;
  initialType?: EventType;
}) {
  const { onSuccess } = useFormResult();
  const createInstances = useMutation(CreateEventInstancesDocument)[1];
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const { lockEventsByDefault } = useAtomValue(tenantConfigAtom);
  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitIds, setSplitIds] = React.useState<Record<string, string | null>>({});
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(EventForm),
    defaultValues: {
      type: initialType,
      locationId: defaults.locationText ? 'other' : (defaults.locationId ?? 'none'),
      locationText: defaults.locationText,
      isVisible: true,
      isPublic: false,
      isLocked: lockEventsByDefault,
      enableNotes: false,
      capacity: 2,
      instances: [
        {
          since: defaults.since.toISOString(),
          until: defaults.until.toISOString(),
          isCancelled: false,
          trainers: [],
        },
      ],
      trainers: defaults.trainerPersonIds.map((personId) => ({
        itemId: null,
        personId,
        lessonsOffered: 0,
      })),
      registrations: [],
    },
  });

  const type = useWatch({ control, name: 'type' }) ?? initialType;
  const instances = useWatch({ control, name: 'instances' });
  const firstInstance = instances?.[0];
  const lessonRanges =
    firstInstance?.since && firstInstance.until
      ? splitIntoLessonRanges(new Date(firstInstance.since), new Date(firstInstance.until))
      : [];
  const canSplit =
    type === 'LESSON' &&
    lessonRanges.length >= 2 &&
    instances?.filter((instance) => instance.since && instance.until).length === 1;
  const splitRegistrationOptions = React.useMemo(
    () => [
      ...(tenant?.tenant?.couplesList || [])
        .filter((couple) => couple.status === 'ACTIVE')
        .map((couple) => ({
          id: `couple:${couple.id}`,
          label: formatLongCoupleName(couple),
        })),
      ...(tenant?.tenant?.tenantMembershipsList || []).flatMap((x) =>
        x.status === 'ACTIVE' && x.person?.id
          ? [{ id: `person:${x.person.id}`, label: x.person.name }]
          : [],
      ),
    ],
    [tenant],
  );
  const trainerField =
    defaults.trainerPersonIds.length > 0 ? (
      <div className="text-sm text-neutral-11">
        Trenéři:{' '}
        {defaults.trainerPersonIds
          .map((id) => tenant?.tenant?.tenantTrainersList.find((x) => x.person?.id === id)?.person?.name)
          .join(', ')}
      </div>
    ) : (
      <TrainerListElement control={control} name="trainers" />
    );

  React.useEffect(() => {
    if (!canSplit) setSplitLessons(false);
  }, [canSplit]);

  const onSubmit = useAsyncCallback(async (values: EventFormType) => {
    const explicitRanges = values.instances.flatMap((instance) =>
      instance.since && instance.until
        ? [{ since: new Date(instance.since), until: new Date(instance.until) }]
        : [],
    );
    const firstRange = explicitRanges[0];
    if (!firstRange) return;
    const selectedRegistrations = values.registrations
      .filter((registration) => registration.personId || registration.coupleId)
      .map((registration) => ({
        personId: registration.personId || null,
        coupleId: registration.coupleId || null,
      }));
    if (!splitLessons && values.type === 'LESSON') {
      const registrantCount = selectedRegistrations.reduce(
        (sum, r) => sum + (r.coupleId ? 2 : 1),
        0,
      );
      if (registrantCount > 2) throw new Error('Lekce má nejvýš dvě místa');
    }

    const trainers = values.trainers.flatMap((trainer) =>
      trainer.personId
        ? [{ personId: trainer.personId, lessonsOffered: trainer.lessonsOffered ?? 0 }]
        : [],
    );
    const trainerPersonIds = trainers.map(({ personId }) => personId);
    const ranges = splitLessons
      ? splitIntoLessonRanges(firstRange.since, firstRange.until)
      : explicitRanges;
    const location = eventLocationInput(values);

    const instancesToCreate = ranges.map((range) => {
      const [kind, id] =
        splitIds[range.since.toISOString()]?.split(':') ?? [];
      const splitRegistration = splitLessons && id && [{
        personId: kind === 'person' ? id : null,
        coupleId: kind === 'couple' ? id : null,
      }] || [];
      return {
        since: range.since.toISOString(),
        until: range.until.toISOString(),
        type: splitLessons ? 'LESSON' : values.type,
        ...location,
        trainerPersonIds,
        registrations: splitLessons ? splitRegistration : selectedRegistrations,
      };
    });

    const result = await createInstances({
      input: {
        events: splitLessons ? instancesToCreate : instancesToCreate.slice(0, 1),
        parentId,
        pCopies:
          !splitLessons && instancesToCreate.length > 1
            ? instancesToCreate.slice(1)
            : null,
        pName: splitLessons ? null : values.name.trim() || null,
        pCapacity: values.capacity,
        pCapacityUnit: values.capacityUnit,
        pDescription: values.description,
        pSummary: values.summary,
        pFilesLegacy: values.titleImageLegacy,
        pCohortIds: values.cohorts.flatMap(({ cohortId }) =>
          cohortId ? [cohortId] : [],
        ),
        pTrainerLessonsOffered: trainers.map(({ lessonsOffered }) => lessonsOffered),
        pIsVisible: values.isVisible,
        pIsPublic: values.isPublic,
        pIsLocked: values.isLocked,
        pEnableNotes: values.enableNotes,
      },
    });
    if (result.error) throw result.error;
    if (result.data?.quickCreateEventInstances?.eventInstances?.length) onSuccess();
  });

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      {canSplit && (
        <Checkbox
          name="splitLessons"
          checked={splitLessons}
          onChange={() => setSplitLessons((value) => !value)}
          label={`Rozdělit na samostatné lekce? (${lessonRanges.length} x 45 min)`}
        />
      )}

      {splitLessons ? (
        <>
          <DateTimeRangeController
            control={control}
            nameSince="instances.0.since"
            nameUntil="instances.0.until"
            isCamp={false}
          />
          {trainerField}
          <LocationField control={control} />
          <div className="grid gap-1 rounded-md border border-neutral-4 bg-neutral-2 p-2">
            {lessonRanges.map((range) => {
              const key = range.since.toISOString();
              const value = splitIds[key] ?? null;
              return (
                <div
                  key={key}
                  className="flex gap-2 text-sm flex-wrap justify-between items-center"
                >
                  <span className="text-neutral-11">
                    {shortTimeFormatter.format(range.since)} -{' '}
                    {shortTimeFormatter.format(range.until)}
                  </span>
                  <div className="sm:justify-self-end">
                    <ComboboxButton
                      value={value}
                      options={splitRegistrationOptions}
                      placeholder="Volno"
                      buttonClassName={
                        value
                          ? undefined
                          : 'border-green-7 bg-green-3 text-green-11 hover:border-green-7 hover:bg-green-3/80'
                      }
                      onChange={(selected) => {
                        setSplitIds((current) => ({
                          ...current,
                          [key]: selected ?? null,
                        }));
                      }}
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
          <TextFieldElement control={control} name="name" label="Název (nepovinný)" />
          <LocationField control={control} />
          {trainerField}
          <InstanceListElement control={control} />
          <ParticipantListElement control={control} name="registrations" />
        </>
      )}

      <EventAccessFields control={control} type={type} />

      <div className="flex justify-end pt-1">
        <SubmitButton loading={onSubmit.loading}>Vytvořit</SubmitButton>
      </div>
    </form>
  );
}

export function QuickInstanceEditForm({
  instance,
}: {
  instance: EventInstanceWithTrainerFragment;
}) {
  const { onSuccess } = useFormResult();
  const updateInstance = useMutation(UpdateEventInstanceDetailsDocument)[1];
  const [registrationsReady, setRegistrationsReady] = React.useState(false);
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
  });
  const directTrainers = instance.eventInstanceTrainersByInstanceIdList;
  const effectiveTrainerIds = instance.trainersList?.map((trainer) => trainer.personId) ?? [];
  const initialTrainers =
    directTrainers.length > 0
      ? directTrainers.map((trainer) => ({
          itemId: trainer.id,
          personId: trainer.personId,
          lessonsOffered: trainer.lessonsOffered,
        }))
      : effectiveTrainerIds.map((personId) => ({
          itemId: null,
          personId,
          lessonsOffered: 0,
        }));
  const initialCohorts = React.useMemo(
    () =>
      (instance.targetCohortsList ?? []).map((target) => ({
        cohortId: target.cohortId,
      })),
    [instance.targetCohortsList],
  );
  const existingCohorts = React.useMemo(
    () =>
      (instance.targetCohortsList ?? []).map((target) => ({
        id: target.cohortId,
        label: target.cohort?.name ?? `Skupina ${target.cohortId}`,
      })),
    [instance.targetCohortsList],
  );

  const { control, handleSubmit, setValue } = useForm<
    EventFormInput,
    unknown,
    EventFormType
  >({
    resolver: zodResolver(EventForm),
    defaultValues: {
      name: instance.name ?? '',
      type: instance.type ?? 'LESSON',
      locationId: instance.locationText ? 'other' : (instance.location?.id ?? 'none'),
      locationText: instance.locationText ?? '',
      capacity: instance.capacity ?? 0,
      capacityUnit: instance.capacityUnit,
      isVisible: instance.isVisible ?? false,
      isPublic: instance.isPublic ?? false,
      isLocked: instance.isLocked ?? false,
      enableNotes: instance.enableNotes ?? false,
      cohorts: initialCohorts,
      registrations: [],
      instances: [
        {
          itemId: instance.id,
          since: instance.since,
          until: instance.until,
          isCancelled: instance.isCancelled,
          trainers: initialTrainers,
        },
      ],
    },
  });

  const registrations =
    registrationsQuery.data?.eventInstance?.registrations.nodes ?? [];

  React.useEffect(() => {
    const instance = registrationsQuery.data?.eventInstance;
    if (
      !registrationsReady &&
      !registrationsQuery.fetching &&
      !registrationsQuery.error &&
      instance
    ) {
      setValue(
        'registrations',
        instance.registrations.nodes.map((x) => ({
          itemId: x.id,
          personId: x.personId,
          coupleId: x.coupleId,
        })),
      );
      setRegistrationsReady(true);
    }
  }, [registrationsQuery, registrationsReady, setValue]);

  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';

  const onSubmit = useAsyncCallback(async (values: EventFormType) => {
    const edited = values.instances[0];
    if (!edited?.since || !edited.until) return;

    const copies = values.instances
      .slice(1)
      .flatMap((copy) =>
        copy.since && copy.until ? [{ since: copy.since, until: copy.until }] : [],
      );
    if (copies.length > 0 && !registrationsReady) {
      throw new Error('Účastníci ještě nejsou načteni');
    }

    const nextTrainers = edited.trainers.flatMap((trainer) =>
      trainer.personId
        ? [{ personId: trainer.personId, lessonsOffered: trainer.lessonsOffered }]
        : [],
    );
    const currentTrainers =
      directTrainers.length > 0
        ? directTrainers.map((trainer) => ({
            personId: trainer.personId,
            lessonsOffered: trainer.lessonsOffered,
          }))
        : effectiveTrainerIds.map((personId) => ({ personId, lessonsOffered: 0 }));
    const trainersChanged =
      JSON.stringify(nextTrainers.toSorted((a, b) => a.personId.localeCompare(b.personId))) !==
      JSON.stringify(currentTrainers.toSorted((a, b) => a.personId.localeCompare(b.personId)));
    const location = eventLocationInput(values);
    const nextRegistrations = values.registrations
      .filter((x) => x.personId || x.coupleId)
      .map((x) => ({
        personId: x.personId || null,
        coupleId: x.coupleId || null,
      }));
    const nextCohortIds = values.cohorts.flatMap(({ cohortId }) =>
      cohortId ? [cohortId] : [],
    );
    const cohortsChanged =
      JSON.stringify(nextCohortIds.toSorted()) !==
      JSON.stringify(instance.targetCohortsList.map((target) => target.cohortId).toSorted());
    const copyEvents = copies.map(({ since, until }) => ({
      since,
      until,
      type: values.type,
      ...location,
      trainerPersonIds: nextTrainers.map((trainer) => trainer.personId),
      registrations: nextRegistrations,
    }));
    const result = await updateInstance({
      input: {
        pInstanceId: instance.id,
        pSince: edited.since,
        pUntil: edited.until,
        pName: values.name.trim() || null,
        pType: values.type,
        pLocationId: location.locationId,
        pLocationText: location.locationText,
        pIsVisible: values.isVisible,
        pIsPublic: values.isPublic,
        pIsCancelled: edited.isCancelled,
        pIsLocked: values.isLocked,
        pEnableNotes: values.enableNotes,
        pTrainerPersonIds: trainersChanged || copies.length > 0
          ? nextTrainers.map((trainer) => trainer.personId)
          : null,
        pTrainerLessonsOffered: trainersChanged || copies.length > 0
          ? nextTrainers.map((trainer) => trainer.lessonsOffered)
          : null,
        pRegistrations: registrationsReady ? nextRegistrations : null,
        pCohortIds: cohortsChanged || copies.length > 0 ? nextCohortIds : null,
        pCopies: copyEvents.length > 0 ? copyEvents : null,
      },
    });
    if (result.error) throw result.error;
    if (result.data?.updateEventInstanceDetails?.eventInstance?.id) onSuccess();
  });

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <RadioButtonGroupElement control={control} name="type" options={eventTypeOptions} />
      <TextFieldElement control={control} name="name" label="Název termínu" />
      {instance.seriesId ? (
        <DateTimeRangeController
          control={control}
          nameSince="instances.0.since"
          nameUntil="instances.0.until"
          isCamp={type === 'CAMP'}
        />
      ) : (
        <InstanceListElement control={control} />
      )}
      <LocationField control={control} />
      <InstanceTrainerListElement control={control} index={0} />
      <CohortListElement
        control={control}
        name="cohorts"
        existingCohorts={existingCohorts}
      />
      {/*type !== 'LESSON' && (
        <>
          <TextFieldElement
            control={control}
            type="number"
            min={0}
            name="capacity"
            label="Kapacita"
          />
          <RadioButtonGroupElement
            control={control}
            name="capacityUnit"
            options={[
              { id: 'PEOPLE', label: 'osob' },
              { id: 'REGISTRATIONS', label: 'přihlášek' },
            ]}
          />
        </>
      )*/}
      {registrationsReady && (
        <ParticipantListElement
          control={control}
          name="registrations"
          existingPeople={registrations.flatMap(({ person }) =>
            person ? [{ id: person.id, label: person.name }] : [],
          )}
          existingCouples={registrations.flatMap(({ couple }) =>
            couple ? [{ id: couple.id, label: formatLongCoupleName(couple) }] : [],
          )}
        />
      )}
      {!registrationsReady && registrationsQuery.fetching && (
        <div className="text-sm text-neutral-11">Načítám účastníky…</div>
      )}
      <EventAccessFields control={control} type={type} />
      <CheckboxElement control={control} name="instances.0.isCancelled" label="Zrušeno" />
      <FormError error={registrationsQuery.error} />

      <div className="flex justify-end pt-1">
        <SubmitButton loading={onSubmit.loading}>Uložit</SubmitButton>
      </div>
    </form>
  );
}
