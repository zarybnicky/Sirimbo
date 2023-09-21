import { SlotInfo } from '@/calendar/SelectContext';
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
import React from 'react';
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
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { diff } from 'date-arithmetic';

export function UpsertEventForm({ onSuccess, slot, event }: {
  slot?: SlotInfo;
  event?: EventFragment;
  onSuccess?: () => void;
}) {
  const upsert = useMutation(UpsertEventDocument)[1];
  const id = event?.id ?? '';
  const [{ data: eventData }, fetchEvent] = useQuery({ query: EventDocument, variables: { id }, pause: true });
  const [{ data: tenantData }] = useQuery({ query: CurrentTenantDocument });

  const { reset, control, handleSubmit, watch, setValue, getValues } = useZodForm(EventForm);

  const locationOptions = React.useMemo(() => {
    return [{ id: 'none', label: 'Žádné' } as RadioButtonGroupItem].concat(
      (tenantData?.tenant?.tenantLocationsList || []).map(x => ({
        id: x.id,
        label: x.name,
      })),
    ).concat({ id: 'other', label: 'Jiné...' });
  }, [tenantData]);

  React.useEffect(() => {
    if (slot) {
      const def: Partial<TypeOf<typeof EventForm>> = {
        instances: [
          datetimeRangeToTimeRange(slot.start, slot.end)
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
        })),
      });
    }
  }, [eventData]);

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
  }, [locationId]);

  React.useEffect(() => {
    if (type === 'LESSON') {
      setValue('capacity', 2);
      setValue('paymentType', 'AFTER_INSTANCE');
      setValue('useDefaultPrice', true);
    } else {
      setValue('capacity', 0);
      setValue('paymentType', 'UPFRONT');
      setValue('useDefaultPrice', false);
      setValue('memberPrice', null);
      setValue('guestPrice', null);
    }
  }, [type]);

  React.useEffect(() => {
    if (getValues('useDefaultPrice') && paymentType !== 'NONE') {
      let memberPrice = 0;
      let guestPrice = 0;
      getValues('trainers')?.forEach(x => {
        const trainer = tenantData?.tenant?.tenantTrainersList.find(p => p.person?.id === x.personId);
        const numericMember = parseInt(trainer?.memberPrice45Min?.amount);
        const numericGuest = parseInt(trainer?.guestPrice45Min?.amount);
        memberPrice += Number.isNaN(numericMember) ? 0 : numericMember;
        guestPrice += Number.isNaN(numericGuest) ? 0 : numericGuest;
      })

      let multiplier = 0;
      if (paymentType === 'UPFRONT') {
        instances?.forEach(x => {
          const range = timeRangeToDatetimeRange(x);
          if (range.since && range.until) {
            multiplier += diff(range.since, range.until, 'minutes') / 45;
          }
        });
      } else {
        const range = instances?.[0] ? timeRangeToDatetimeRange(instances[0]) : null;
        if (range?.since && range.until) {
          multiplier = diff(range.since, range.until, 'minutes') / 45;
        } else {
          multiplier = 1;
        }
      }
      memberPrice = !Number.isNaN(memberPrice) ? (memberPrice * multiplier) : 0;
      guestPrice = !Number.isNaN(guestPrice) ? (guestPrice * multiplier) : 0;
      memberPrice = Math.floor(memberPrice / 10) * 10;
      guestPrice = Math.floor(guestPrice / 10) * 10;
      setValue('memberPrice', memberPrice);
      setValue('guestPrice', guestPrice);
    }
  }, [trainers, instances, paymentType]);

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
          guestPrice: values.guestPrice ? {amount: values.guestPrice, currency: 'CZK'} : null,
          memberPrice: values.memberPrice ? {amount: values.memberPrice, currency: 'CZK'} : null,
          useDefaultPrice: values.useDefaultPrice,
          paymentType: values.paymentType,
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

      {!!memberPrice && (
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

  if (!perms.isAdmin) return null;

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
