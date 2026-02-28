import { EventDocument, type EventFragment, UpsertEventDocument } from '@/graphql/Event';
import {
  RadioButtonGroupElement,
  type RadioButtonGroupItem,
} from '@/ui/fields/RadioButtonGroupElement';
import { CohortListElement } from '@/ui/event-form/CohortListElement';
import { InstanceListElement } from '@/ui/event-form/InstanceListElement';
import { ParticipantListElement } from '@/ui/event-form/ParticipantListElement';
import { TrainerListElement } from '@/ui/event-form/TrainerListField';
import { EventForm } from '@/ui/event-form/types';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { TextFieldElement } from '@/ui/fields/text';
import { moneyFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { diff } from 'date-arithmetic';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useFormResult } from '@/ui/form';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from '@/ui/state/auth';
import { CurrentTenantDocument } from '@/graphql/Tenant';

type NonEmptyArray<T> = [T, ...T[]];
const isNonEmpty = <T,>(array: Array<T> | null | undefined): array is NonEmptyArray<T> =>
  !!array?.length;

export function UpsertEventForm({
  initialValue = {},
  event,
}: {
  initialValue?: Partial<z.input<typeof EventForm>>;
  event?: EventFragment;
}) {
  const { lockEventsByDefault } = useAtomValue(tenantConfigAtom);
  const initializedRef = React.useRef(false);
  const { onSuccess } = useFormResult();
  const upsert = useMutation(UpsertEventDocument)[1];
  const id = event?.id ?? '';
  const [{ data: eventData }, fetchEvent] = useQuery({
    query: EventDocument,
    variables: { id },
    pause: true,
  });
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });

  const { reset, control, handleSubmit, watch, setValue, getValues } = useForm<
    z.input<typeof EventForm>,
    unknown,
    z.infer<typeof EventForm>
  >({
    resolver: zodResolver(EventForm),
    defaultValues: {
      ...initialValue,
      isLocked: initialValue.isLocked ?? lockEventsByDefault,
    },
  });

  const locationOptions = React.useMemo(() => {
    return [
      { id: 'none', label: 'Žádné' } as RadioButtonGroupItem,
      ...(tenant?.tenant?.tenantLocationsList || []).map((x) => ({
        id: x.id,
        label: x.name,
      })),
      { id: 'other', label: 'Jiné...' },
    ];
  }, [tenant]);

  React.useEffect(() => {
    if (event && !initializedRef.current) {
      fetchEvent();
    }
  }, [event, fetchEvent]);

  React.useEffect(() => {
    const event = eventData?.event;
    if (event && !initializedRef.current) {
      initializedRef.current = true;
      reset(
        {
          ...event,
          locationId: event.locationText ? 'other' : (event.location?.id ?? 'none'),
          trainers: event.eventTrainersList.map((x) => ({
            itemId: x.id,
            personId: x.personId,
            lessonsOffered: x.lessonsOffered,
          })),
          cohorts: event.eventTargetCohortsList.map((x) => ({
            itemId: x.id,
            cohortId: x.cohort?.id ?? '',
          })),
          registrations: event.eventRegistrationsList.map((x) => ({
            itemId: x.id,
            coupleId: x.coupleId,
            personId: x.personId,
          })),
          instances: event.eventInstancesList.map((x) => ({
            itemId: x.id,
            since: x.since,
            until: x.until,
            isCancelled: x.isCancelled,
            trainers: x.eventInstanceTrainersByInstanceIdList.map((y) => ({
              itemId: y.id,
              personId: y.personId,
            })),
          })),
        },
        {
          keepDirtyValues: true,
          keepTouched: true,
          keepErrors: true,
        },
      );
    }
  }, [reset, eventData]);

  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';
  const trainers = useWatch({ control, name: 'trainers' });
  const instances = useWatch({ control, name: 'instances' });
  const registrations = useWatch({ control, name: 'registrations' });
  const locationId = useWatch({ control, name: 'locationId' });
  const registrantCount = (registrations || []).reduce(
    (n, x) => n + (x.coupleId ? 2 : x.personId ? 1 : 0),
    0,
  );

  const memberPrice = React.useMemo(() => {
    let memberPrice = 0;
    for (const x of trainers || []) {
      const trainer = tenant?.tenant?.tenantTrainersList.find(
        (p) => p.person?.id === x.personId,
      );
      const numericMember = Number.parseFloat(trainer?.memberPrice45MinAmount || '');
      memberPrice += Number.isNaN(numericMember) ? 0 : numericMember;
    }

    let multiplier: number;
    if (isNonEmpty(instances) && instances[0].since && instances[0].until) {
      const { since, until } = instances[0];
      multiplier = diff(new Date(since), new Date(until), 'minutes') / 45;
    } else {
      multiplier = 1;
    }

    memberPrice = Number.isNaN(memberPrice) ? 0 : memberPrice * multiplier;
    return Math.floor(memberPrice / 10) * 10;
  }, [instances, trainers, tenant?.tenant?.tenantTrainersList]);

  React.useEffect(() => {
    if (locationId !== 'other' && getValues('locationText')) {
      setValue('locationText', '');
    }
  }, [getValues, setValue, locationId]);

  React.useEffect(() => {
    setValue('capacity', type === 'LESSON' ? 2 : 0);
  }, [setValue, type]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof EventForm>) => {
    const result = await upsert({
      input: {
        info: {
          id: event?.id || null,
          name: values.name,
          summary: values.summary,
          description: values.description,
          type: values.type,
          locationId:
            !values.locationId || ['none', 'other'].includes(values.locationId)
              ? null
              : values.locationId,
          locationText: values.locationId === 'none' ? '' : values.locationText,
          capacity: values.capacity,
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
        },
        trainers: values.trainers.map((x) => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        cohorts: values.cohorts.map((x) => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        registrations: values.registrations.map((x) => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        instances: values.instances.map((x) => ({
          id: x.itemId,
          since: x.since,
          until: x.until,
          isCancelled: x.isCancelled,
          trainers: x.trainers.map((y) => ({
            ...y,
            id: y.itemId,
            itemId: undefined,
          })),
        })),
      },
    });
    if (result.data?.upsertEvent?.event?.id) {
      onSuccess();
    }
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <RadioButtonGroupElement
        control={control}
        name="type"
        options={[
          { id: 'LESSON', label: 'Lekce' },
          { id: 'GROUP', label: 'Společná' },
          { id: 'RESERVATION', label: 'Nabídka' },
          { id: 'CAMP', label: 'Soustředění' },
          /* { id: 'HOLIDAY', label: 'Prázdniny' }, */
        ]}
      />

      <TextFieldElement control={control} name="name" label="Název (nepovinný)" />
      <RadioButtonGroupElement
        control={control}
        name="locationId"
        options={locationOptions}
        label="Místo konání"
      />
      {locationId === 'other' && (
        <TextFieldElement
          control={control}
          name="locationText"
          placeholder="Místo konání"
        />
      )}
      {type !== 'LESSON' && (
        <TextFieldElement
          control={control}
          type="number"
          name="capacity"
          label="Maximální počet účastníků (nepovinný)"
        />
      )}

      <InstanceListElement control={control} name="instances" />
      <TrainerListElement control={control} name="trainers" />

      {!!memberPrice && type === 'LESSON' && (
        <div className="">
          {'Cena: '}
          {moneyFormatter.format({ amount: memberPrice.toString(), currency: 'CZK' })}
          {registrantCount > 0 && (
            <>
              {', na účastníka '}
              {moneyFormatter.format({
                amount: Math.floor(memberPrice / registrantCount).toString(),
                currency: 'CZK',
              })}
            </>
          )}
        </div>
      )}

      <CohortListElement control={control} name="cohorts" />
      <ParticipantListElement control={control} name="registrations" />

      {/* <RadioButtonGroupElement
        control={control}
        name="titleImageLegacy"
        options={[
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671943-Akce-titulni1.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671943-Akce-titulni1.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni2.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni2.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni3.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni3.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni4.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni4.jpg" />
          },
        ]}
      /> */}

      <div className="flex gap-x-1 flex-wrap items-baseline justify-between">
        <CheckboxElement
          control={control}
          name="isVisible"
          value="1"
          label="Viditelná pro členy"
        />
        <CheckboxElement
          control={control}
          name="isLocked"
          value="1"
          label="Zakázat přihlašování/odhlašování"
        />
        <CheckboxElement
          control={control}
          name="isPublic"
          value="1"
          label="Viditelná pro veřejnost"
        />
        {(type === 'RESERVATION' || type === 'CAMP') && (
          <CheckboxElement
            control={control}
            name="enableNotes"
            value="1"
            label="Povolit poznámky k přihlášce"
          />
        )}
      </div>

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
