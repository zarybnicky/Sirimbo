import { EventDocument, UpsertEventDocument } from '@/graphql/Event';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { InstanceListElement } from '@/ui/event-form/InstanceListElement';
import { eventLocationInput, LocationField } from '@/ui/event-form/LocationField';
import { EventForm } from '@/ui/event-form/types';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { TextFieldElement } from '@/ui/fields/text';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useFormResult } from '@/ui/form';
import { useForm, useWatch } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from '@/ui/state/auth';

export function UpsertEventForm({
  initialValue = {},
  eventId,
}: {
  initialValue?: Partial<z.input<typeof EventForm>>;
  eventId?: string;
}) {
  const { lockEventsByDefault } = useAtomValue(tenantConfigAtom);
  const initializedRef = React.useRef(false);
  const { onSuccess } = useFormResult();
  const upsert = useMutation(UpsertEventDocument)[1];
  const id = eventId ?? '';
  const [{ data: eventData }, fetchEvent] = useQuery({
    query: EventDocument,
    variables: { id },
    pause: true,
  });

  const { reset, control, handleSubmit, setValue } = useForm<
    z.input<typeof EventForm>,
    unknown,
    z.infer<typeof EventForm>
  >({
    resolver: zodResolver(EventForm),
    defaultValues: {
      ...initialValue,
      trainers: [],
      isLocked: initialValue.isLocked ?? lockEventsByDefault,
    },
  });

  React.useEffect(() => {
    if (eventId && !initializedRef.current) {
      fetchEvent();
    }
  }, [eventId, fetchEvent]);

  React.useEffect(() => {
    const event = eventData?.event;
    if (event && !initializedRef.current) {
      initializedRef.current = true;
      reset(
        {
          ...event,
          locationId: event.locationText ? 'other' : (event.location?.id ?? 'none'),
          trainers: [],
          instances: event.eventInstancesList.map((x) => ({
            itemId: x.id,
            since: x.since,
            until: x.until,
            isCancelled: x.isCancelled,
            trainers: x.eventInstanceTrainersByInstanceIdList.map((y) => ({
              itemId: y.id,
              personId: y.personId,
              lessonsOffered: y.lessonsOffered,
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

  React.useEffect(() => {
    setValue('capacity', type === 'LESSON' ? 2 : 0);
  }, [setValue, type]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof EventForm>) => {
    const result = await upsert({
      input: {
        info: {
          id: eventId || null,
          name: values.name,
          summary: values.summary,
          description: values.description,
          type: values.type,
          ...eventLocationInput(values),
          capacity: values.capacity,
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
        },
        trainers: [],
        eventInstances: values.instances.map((x) => ({
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
      <LocationField control={control} />
      {type !== 'LESSON' && (
        <TextFieldElement
          control={control}
          type="number"
          name="capacity"
          label="Maximální počet účastníků (nepovinný)"
        />
      )}

      <InstanceListElement control={control} name="instances" />

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
