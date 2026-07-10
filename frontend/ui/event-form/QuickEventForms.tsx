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
import { buttonCls } from '@/ui/style';
import { zodResolver } from '@hookform/resolvers/zod';
import { ChevronDown } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm, useWatch } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { DateTimeRangeController } from './InstanceListElement';
import { eventLocationInput, LocationField } from './LocationField';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListField';
import { EventForm, type EventFormType } from './types';
import { UpsertEventForm } from './UpsertEventForm';

const eventTypeOptions: RadioButtonGroupItem[] = [
  'LESSON',
  'GROUP',
  'RESERVATION',
  'CAMP',
  'HOLIDAY',
].map((type) => ({ id: type, label: formatEventType(type as EventType) }));

type EventFormInput = z.input<typeof EventForm>;

export function QuickEventCreateForm({
  defaults,
  parentId,
}: {
  defaults: QuickEventCreateDefaults;
  parentId?: string;
}) {
  const { onSuccess } = useFormResult();
  const createInstances = useMutation(CreateEventInstancesDocument)[1];
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const [splitLessons, setSplitLessons] = React.useState(false);
  const [splitRegistrationIds, setSplitRegistrationIds] = React.useState<
    Record<string, string | null>
  >({});
  const [fullInitialValue, setFullInitialValue] =
    React.useState<Partial<EventFormInput> | null>(null);
  const { control, handleSubmit, getValues } = useForm<
    EventFormInput,
    unknown,
    EventFormType
  >({
    resolver: zodResolver(EventForm),
    defaultValues: {
      type: 'LESSON',
      locationId: defaults.locationText ? 'other' : (defaults.locationId ?? 'none'),
      locationText: defaults.locationText,
      isVisible: true,
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

  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';
  const instances = useWatch({ control, name: 'instances' });
  const firstInstance = instances?.[0];
  const lessonRanges =
    firstInstance?.since && firstInstance.until
      ? splitIntoLessonRanges(new Date(firstInstance.since), new Date(firstInstance.until))
      : [];
  const canSplit = type === 'LESSON' && lessonRanges.length >= 2;
  const splitRegistrationOptions = React.useMemo(
    () => [
      ...(tenant?.tenant?.couplesList || [])
        .filter((couple) => couple.status === 'ACTIVE')
        .map((couple) => ({
          id: `couple:${couple.id}`,
          label: formatLongCoupleName(couple),
        })),
      ...(tenant?.tenant?.tenantMembershipsList || []).flatMap((membership) =>
        membership.status === 'ACTIVE' && membership.person?.id
          ? [
              {
                id: `person:${membership.person.id}`,
                label: membership.person.name || '?',
              },
            ]
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
          .map(
            (personId) =>
              tenant?.tenant?.tenantTrainersList.find(
                (trainer) => trainer.person?.id === personId,
              )?.person?.name ?? personId,
          )
          .join(', ')}
      </div>
    ) : (
      <TrainerListElement control={control} name="trainers" />
    );

  React.useEffect(() => {
    if (!canSplit) setSplitLessons(false);
  }, [canSplit]);

  const onSubmit = useAsyncCallback(async (values: EventFormType) => {
    const instance = values.instances[0];
    if (!instance?.since || !instance.until) return;
    const selectedRegistrations = values.registrations
      .filter((registration) => registration.personId || registration.coupleId)
      .map((registration) => ({
        personId: registration.personId || null,
        coupleId: registration.coupleId || null,
      }));
    if (!splitLessons && values.type === 'LESSON') {
      const registrantCount = selectedRegistrations.reduce(
        (sum, registration) => sum + (registration.coupleId ? 2 : 1),
        0,
      );
      if (registrantCount > 2) throw new Error('Lekce má nejvýš dvě místa');
    }

    const trainerPersonIds = values.trainers
      .map((trainer) => trainer.personId)
      .filter((id): id is string => !!id);
    const ranges = splitLessons
      ? splitIntoLessonRanges(new Date(instance.since), new Date(instance.until))
      : [{ since: new Date(instance.since), until: new Date(instance.until) }];
    const location = eventLocationInput(values);

    const events = ranges.map((range) => {
      const [registrationKind, registrationId] =
        splitRegistrationIds[range.since.toISOString()]?.split(':') ?? [];
      const splitRegistration =
        splitLessons &&
        registrationId &&
        (registrationKind === 'person' || registrationKind === 'couple')
          ? [
              {
                personId: registrationKind === 'person' ? registrationId : null,
                coupleId: registrationKind === 'couple' ? registrationId : null,
              },
            ]
          : [];
      return {
        since: range.since.toISOString(),
        until: range.until.toISOString(),
        type: splitLessons ? 'LESSON' : values.type,
        ...location,
        trainerPersonIds,
        registrations: splitLessons ? splitRegistration : selectedRegistrations,
      };
    });

    const result = await createInstances({ input: { events, parentId } });
    if (result.error) throw result.error;
    if (result.data?.quickCreateEventInstances?.eventInstances?.length) onSuccess();
  });

  if (fullInitialValue) {
    return <UpsertEventForm initialValue={fullInitialValue} />;
  }

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
              const value = splitRegistrationIds[key] ?? null;
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
                        setSplitRegistrationIds((current) => ({
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
          <DateTimeRangeController
            control={control}
            nameSince="instances.0.since"
            nameUntil="instances.0.until"
            isCamp={type === 'CAMP'}
          />
          {trainerField}
          <LocationField control={control} />
          <ParticipantListElement control={control} name="registrations" />
        </>
      )}

      <div className="flex items-center justify-between gap-2 pt-1">
        {parentId ? (
          <span />
        ) : (
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => setFullInitialValue(getValues())}
          >
            Více možností
            <ChevronDown />
          </button>
        )}
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
  const [registrationsReady, setRegistrationsReady] = React.useState(!!instance.eventId);
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
    pause: !!instance.eventId,
  });
  const [fullForm, setFullForm] = React.useState(false);
  const directTrainers = instance.eventInstanceTrainersByInstanceIdList;
  const effectiveTrainerIds = instance.trainersList?.map((trainer) => trainer.personId) ?? [];
  const initialTrainers =
    directTrainers.length > 0
      ? directTrainers.map((trainer) => ({ itemId: trainer.id, personId: trainer.personId }))
      : effectiveTrainerIds.map((personId) => ({ itemId: null, personId }));

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
      isLocked: instance.isLocked ?? false,
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
        instance.registrations.nodes.map((registration) => ({
          itemId: registration.id,
          personId: registration.personId,
          coupleId: registration.coupleId,
        })),
      );
      setRegistrationsReady(true);
    }
  }, [registrationsQuery, registrationsReady, setValue]);

  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';

  const onSubmit = useAsyncCallback(async (values: EventFormType) => {
    const edited = values.instances[0];
    if (!edited?.since || !edited.until) return;

    const nextTrainerIds = edited.trainers
      .map((trainer) => trainer.personId)
      .filter((id): id is string => !!id);
    const currentTrainerIds =
      directTrainers.length > 0
        ? directTrainers.map((trainer) => trainer.personId)
        : effectiveTrainerIds;
    const sameTrainers =
      nextTrainerIds.toSorted().join(',') === currentTrainerIds.toSorted().join(',');
    const location = eventLocationInput(values);
    const nextRegistrations = values.registrations
      .filter((registration) => registration.personId || registration.coupleId)
      .map((registration) => ({
        personId: registration.personId || null,
        coupleId: registration.coupleId || null,
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
        pIsVisible: null,
        pIsPublic: null,
        pIsCancelled: edited.isCancelled,
        pCapacity: values.type === 'LESSON' ? 2 : values.capacity,
        pCapacityUnit: values.type === 'LESSON' ? 'PEOPLE' : values.capacityUnit,
        pIsLocked: values.isLocked,
        pTrainerPersonIds: sameTrainers ? null : nextTrainerIds,
        pRegistrations: instance.eventId || !registrationsReady ? null : nextRegistrations,
      },
    });
    if (result.error) throw result.error;
    if (result.data?.updateEventInstanceDetails?.eventInstance?.id) onSuccess();
  });

  if (fullForm && instance.eventId) {
    return <UpsertEventForm eventId={instance.eventId} />;
  }

  return (
    <form className="space-y-3" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <RadioButtonGroupElement control={control} name="type" options={eventTypeOptions} />
      <TextFieldElement control={control} name="name" label="Název termínu" />
      <DateTimeRangeController
        control={control}
        nameSince="instances.0.since"
        nameUntil="instances.0.until"
        isCamp={type === 'CAMP'}
      />
      <TrainerListElement control={control} name="trainers" />
      <LocationField control={control} />
      {type !== 'LESSON' && (
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
      )}
      <CheckboxElement
        control={control}
        name="isLocked"
        label="Zakázat přihlašování/odhlašování"
      />
      {!instance.eventId && registrationsReady && (
        <ParticipantListElement
          control={control}
          name="registrations"
          existingPeople={registrations.flatMap(({ personId: id, person }) =>
            id
              ? [{ id, label: person?.name ?? `Účastník #${id}` }]
              : [],
          )}
          existingCouples={registrations.flatMap(({ coupleId: id, couple }) =>
            id
              ? [
                  {
                    id,
                    label: formatLongCoupleName(couple) || `Pár #${id}`,
                  },
                ]
              : [],
          )}
        />
      )}
      {!instance.eventId && !registrationsReady && registrationsQuery.fetching && (
        <div className="text-sm text-neutral-11">Načítám účastníky…</div>
      )}
      <FormError error={registrationsQuery.error} />
      <CheckboxElement
        control={control}
        name="instances.0.isCancelled"
        label="Zrušený termín"
      />

      <div className="flex items-center justify-between gap-2 pt-1">
        {instance.eventId ? (
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => setFullForm(true)}
          >
            Více možností
            <ChevronDown />
          </button>
        ) : (
          <span />
        )}
        <SubmitButton loading={onSubmit.loading}>Uložit</SubmitButton>
      </div>
    </form>
  );
}
