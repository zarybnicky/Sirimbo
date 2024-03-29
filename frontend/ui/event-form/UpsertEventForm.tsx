import { EventType } from '@/graphql';
import { EventDocument, EventFragment, UpsertEventDocument } from '@/graphql/Event';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement, RadioButtonGroupItem } from '@/ui/RadioButtomGroupElement';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { TextFieldElement } from '@/ui/fields/text';
import { datetimeRangeToTimeRange, timeRangeToDatetimeRange } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { Pencil } from 'lucide-react';
import React, { useState } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { TypeOf } from 'zod';
import { CohortListElement } from './CohortListElement';
import { InstanceListElement } from './InstanceListElement';
import { ParticipantListElement } from './ParticipantListElement';
import { TrainerListElement } from './TrainerListField';
import { EventForm } from './types';
import { cn } from '../cn';
import { useAuth } from '../use-auth';
import { add, diff, endOf, startOf } from 'date-arithmetic';
import { SlotInfo } from '@/calendar/types';
import { buttonCls } from '../style';
import { useTenant } from '../useTenant';

export function UpsertEventForm({ onSuccess, slot, event }: {
  slot?: SlotInfo;
  event?: EventFragment;
  onSuccess?: () => void;
}) {
  const upsert = useMutation(UpsertEventDocument)[1];
  const id = event?.id ?? '';
  const [{ data: eventData }, fetchEvent] = useQuery({ query: EventDocument, variables: { id }, pause: true });
  const { data: tenant } = useTenant();

  const { reset, control, handleSubmit, watch, setValue, getValues } = useZodForm(EventForm);

  const locationOptions = React.useMemo(() => {
    return [{ id: 'none', label: 'Žádné' } as RadioButtonGroupItem].concat(
      (tenant?.tenantLocationsList || []).map(x => ({
        id: x.id,
        label: x.name,
      })),
    ).concat({ id: 'other', label: 'Jiné...' });
  }, [tenant]);

  React.useEffect(() => {
    if (slot) {
      const def: Partial<TypeOf<typeof EventForm>> = {
        instances: [
          {
            ...(datetimeRangeToTimeRange(slot.start, slot.end)),
            isCancelled: false,
          }
        ],
        isVisible: true,
        type: 'LESSON' as EventType,
        capacity: 2,
        locationId: 'none'
      };
      const [resourceType, id] = slot.resourceId?.split('-', 2) || [];
      if (resourceType === 'location' && id) {
        def.locationId = id;
      }
      if (resourceType === 'locationText' && id) {
        def.locationId = 'other';
        def.locationText = id;
      }
      if (resourceType === 'person' && id) {
        def.trainers = [{ itemId: null, personId: id, lessonsOffered: 0 }];
      }
      reset(def);
    } else if (event) {
      fetchEvent();
    };
  }, [slot]);

  React.useEffect(() => {
    if (eventData?.event) {
      const event = eventData.event;
      reset({
        ...event,
        locationId: event.locationText ? 'other' : event.location?.id ?? 'none',
        guestPrice: event.guestPrice?.amount,
        memberPrice: event.memberPrice?.amount,
        trainers: event.eventTrainersList.map(x => ({
          itemId: x.id,
          personId: x.person?.id,
          lessonsOffered: x.lessonsOffered,
        })),
        cohorts: event.eventTargetCohortsList.map(x => ({
          itemId: x.id,
          cohortId: x.cohort?.id ?? '',
        })),
        registrations: event.eventRegistrationsList.map(x => ({
          itemId: x.id,
          coupleId: x.coupleId,
          personId: x.personId,
          isConfirmed: x.isConfirmed ?? false,
        })),
        instances: event.eventInstancesList.map(x => ({
          itemId: x.id,
          ...datetimeRangeToTimeRange(new Date(x.since), new Date(x.until)),
          isCancelled: x.isCancelled
        })),
      });
    }
  }, [reset, eventData]);

  const type = watch('type');
  const trainers = watch('trainers');
  const instances = watch('instances');
  const registrations = watch('registrations');
  const paymentType = watch('paymentType');
  const memberPrice = watch('memberPrice');
  const locationId = watch('locationId');

  const registrantCount = (registrations || []).reduce((n, x) => n + (x.coupleId ? 2 : x.personId ? 1 : 0), 0);

  React.useEffect(() => {
    if (locationId !== 'other' && getValues('locationText')) {
      setValue('locationText', '');
    }
  }, [getValues, setValue, locationId]);

  React.useEffect(() => {
    if (type === 'LESSON') {
      setValue('capacity', 2);
      setValue('paymentType', 'AFTER_INSTANCE');
    } else {
      setValue('capacity', 0);
      setValue('paymentType', 'NONE');
      setValue('memberPrice', null);
      setValue('guestPrice', null);
    }
  }, [setValue, type]);

  React.useEffect(() => {
    let memberPrice = 0;
    let guestPrice = 0;
    getValues('trainers')?.forEach(x => {
      const trainer = tenant?.tenantTrainersList.find(p => p.person?.id === x.personId);
      const numericMember = parseInt(trainer?.memberPrice45Min?.amount);
      const numericGuest = parseInt(trainer?.guestPrice45Min?.amount);
      memberPrice += Number.isNaN(numericMember) ? 0 : numericMember;
      guestPrice += Number.isNaN(numericGuest) ? 0 : numericGuest;
    })

    let multiplier = 0;
    const range = instances?.[0] ? timeRangeToDatetimeRange(instances[0]) : null;
    if (range?.since && range.until) {
      multiplier = diff(range.since, range.until, 'minutes') / 45;
    } else {
      multiplier = 1;
    }

    memberPrice = !Number.isNaN(memberPrice) ? (memberPrice * multiplier) : 0;
    guestPrice = !Number.isNaN(guestPrice) ? (guestPrice * multiplier) : 0;
    memberPrice = Math.floor(memberPrice / 10) * 10;
    guestPrice = Math.floor(guestPrice / 10) * 10;
    setValue('memberPrice', memberPrice);
    setValue('guestPrice', guestPrice);
  }, [getValues, setValue, trainers, instances, paymentType]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof EventForm>) => {
    const result = await upsert({
      input: {
        info: {
          id: event?.id || null,
          name: values.name,
          summary: values.summary,
          description: values.description,
          descriptionMember: values.descriptionMember,
          filesLegacy: values.filesLegacy,
          type: values.type,
          locationId: (!values.locationId || ['none', 'other'].includes(values.locationId)) ? null : values.locationId,
          locationText: values.locationId === 'none' ? '' : values.locationText,
          capacity: values.capacity,
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
          guestPrice: null,
          memberPrice: null,
          paymentType: 'NONE',
        },
        trainers: values.trainers.map(x => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        cohorts: values.cohorts.map(x => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        registrations: values.registrations.map(x => ({
          ...x,
          id: x.itemId,
          itemId: undefined,
        })),
        instances: values.instances.map(x => {
          const y = timeRangeToDatetimeRange(x);
          return {
            id: x.itemId,
            since: y.since?.toISOString() || null,
            until: y.until?.toISOString() || null,
            isCancelled: x.isCancelled,
          };
        }),
      },
    });
    if (result.data?.upsertEvent?.event?.id) {
      onSuccess?.();
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
      <RadioButtonGroupElement control={control} name="locationId" options={locationOptions} label="Místo konání" />
      {locationId === 'other' && (
        <TextFieldElement control={control} name="locationText" placeholder="Místo konání" />
      )}
      {type !== 'LESSON' && (
        <TextFieldElement control={control} type="number" name="capacity" label="Maximální počet účastníků (nepovinný)" />
      )}

      <InstanceListElement control={control} name="instances" />
      <TrainerListElement control={control} name="trainers" />

      {!!memberPrice && paymentType !== 'NONE' && (
        <div className="">
          Cena: {memberPrice} Kč
          {!!registrantCount && (
            <>, na účastníka {Math.floor(memberPrice/registrantCount)} Kč</>
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
        <CheckboxElement control={control} name="isVisible" value="1" label="Viditelná pro členy" />
        <CheckboxElement control={control} name="isLocked" value="1" label="Zakázat přihlašování/odhlašování" />
        <CheckboxElement control={control} name="isPublic" value="1" label="Viditelná pro veřejnost" />
        {(type === 'RESERVATION' || type === 'CAMP') && (
          <CheckboxElement control={control} name="enableNotes" value="1" label="Povolit poznámky k přihlášce" />
        )}
      </div>

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};

export function UpsertEventSmallButton({ event, className }: {
  event?: EventFragment;
  className?: string;
}) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);

  if (!perms.isTrainerOrAdmin) return null;

  return (
    <Dialog open={editOpen} onOpenChange={setEditOpen} modal={false}>
      <DialogTrigger asChild>
        <button
          onClick={() => setEditOpen(true)}
          className={cn("rounded-sm opacity-70 ring-offset-neutral-7 transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-accent-7 focus:ring-offset-2 disabled:pointer-events-none data-[state=open]:bg-accent-5 data-[state=open]:text-white", className)}
        >
          <Pencil className="h-4 w-4" />
          <span className="sr-only">Upravit</span>
        </button>
      </DialogTrigger>
      <DialogContent>
        <UpsertEventForm event={event} onSuccess={() => setEditOpen(false)} />
      </DialogContent>
    </Dialog>
  );
}

export const UpsertEventButton = React.memo(function UpsertEventButton({ event }: {
  event?: EventFragment;
  className?: string;
}) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = useState(false);
  const [start] = useState(() => add(startOf(endOf(new Date(), 'week', 1), 'day'), 9, 'hours'));
  const [end] = useState(() => add(startOf(endOf(new Date(), 'week', 1), 'day'), 17, 'hours'));
  const [emptyEvent] = useState(() => ({ start, end, action: 'click' as const, slots: [] }));

  if (!perms.isTrainerOrAdmin) return null;

  return (
    <Dialog open={editOpen} onOpenChange={setEditOpen} modal={false}>
      <DialogTrigger asChild>
        <button
          onClick={() => setEditOpen(true)}
          className={buttonCls({ size: 'sm', variant: 'outline' })}
        >
          <Pencil className="h-4 w-4" />
          Přidat událost
        </button>
      </DialogTrigger>
      <DialogContent>
        <UpsertEventForm
          slot={emptyEvent}
          event={event}
          onSuccess={() => setEditOpen(false)}
        />
      </DialogContent>
    </Dialog>
  );
})
