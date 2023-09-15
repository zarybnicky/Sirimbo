import { SlotInfo } from '@/calendar/SelectContext';
import { EventType } from '@/graphql';
import { EventDocument, EventFragment, UpsertEventDocument } from '@/graphql/Event';
import { useZodForm } from '@/lib/use-schema-form';
import { RadioButtonGroupElement } from '@/ui/RadioButtomGroupElement';
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

 export function UpsertEventForm({ onSuccess, slot, event }: {
  slot?: SlotInfo;
  event?: EventFragment;
  onSuccess?: () => void;
}) {
  const upsert = useMutation(UpsertEventDocument)[1];
  const id = event?.id ?? '';
  const [{ data: eventData }, fetchEvent] = useQuery({ query: EventDocument, variables: { id }, pause: true });

  const { reset, control, handleSubmit, watch, setValue } = useZodForm(EventForm);

  React.useEffect(() => {
    if (slot) {
      const def: Partial<TypeOf<typeof EventForm>> = {
        instances: [
          datetimeRangeToTimeRange(slot.start, slot.end)
        ],
        isVisible: true,
        type: 'LESSON' as EventType,
        capacity: 2,
      };
      const [resourceType, id] = slot.resourceId?.split('-', 2) || [];
      if (resourceType === 'locationText' && id) {
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
  React.useEffect(() => {
    setValue('capacity', type === 'LESSON' ? 2 : 0);
  }, [type]);

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
          locationText: values.locationText,
          capacity: values.capacity,
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
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
      <TextFieldElement control={control} name="locationText" label="Místo konání" />
      {type !== 'LESSON' && (
        <TextFieldElement control={control} type="number" name="capacity" label="Maximální počet účastníků (nepovinný)" />
      )}

      <InstanceListElement control={control} name="instances" />
      <TrainerListElement control={control} name="trainers" />
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

      <div className="flex gap-2 flex-wrap justify-between">
        <CheckboxElement control={control} name="isVisible" value="1" label="Zviditelnit pro členy" />
        <CheckboxElement control={control} name="isPublic" value="1" label="Zviditelnit pro veřejnost" />
        <CheckboxElement control={control} name="isLocked" value="1" label="Uzamčená" />
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
