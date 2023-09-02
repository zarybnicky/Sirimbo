import { CreateEventDocument } from '@app/graphql/Event';
import React from 'react';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { RadioButtonGroupElement } from './RadioButtomGroupElement';
import { TypeOf, z } from 'zod';
import { useZodForm } from '@/lib/use-schema-form';
import { useFieldArray } from 'react-hook-form';
import { add } from 'date-arithmetic';
import { ComboboxElement } from './Combobox';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { datetimeRangeToTimeRange, formatLongCoupleName, timeRangeToDatetimeRange } from './format';
import { buttonCls } from './style';
import { X, Plus } from 'lucide-react';
import { SubmitButton } from './submit';
import { SlotInfo } from '@/calendar/SelectContext';

const Form = z.object({
  name: z.string().default(''),
  type: z.enum(['CAMP', 'LESSON', 'RESERVATION', 'HOLIDAY', 'GROUP']),
  locationText: z.string().default(''),
  capacity: z.number().nullish().default(0),
  isVisible: z.boolean().default(false),
  isPublic: z.boolean().default(false),
  enableNotes: z.boolean().default(false),
  isLocked: z.boolean().default(false),
  titleImageLegacy: z.string().nullish().default(null),
  instances: z.array(
    z.object({
      id: z.string().nullish(),
      date: z.string(),
      startTime: z.string(),
      endTime: z.string(),
    }),
  ),
  trainers: z.array(
    z.object({
      personId: z.string(),
      lessonsOffered: z.number().nullish().default(null),
    }),
  ).default([]),
  cohorts: z.array(
    z.object({
      cohortId: z.string(),
    }),
  ).default([]),
  registrations: z.array(
    z.object({
      id: z.string(),
    }),
  ).default([]),
});

export const CreateEventForm = ({ onSuccess, ...slot }: SlotInfo & { onSuccess?: () => void }) => {
  const create = useMutation(CreateEventDocument)[1];
  const [tenantQuery] = useQuery({ query: CurrentTenantDocument });

  const { control, handleSubmit, watch, setValue } = useZodForm(Form, {
    defaultValues: {
      instances: [
        datetimeRangeToTimeRange(slot.start, slot.end)
      ],
      trainers: [{ personId: slot.resourceId?.toString() }],
      isVisible: true,
      type: 'LESSON',
    },
  });
  const { fields: instances, append: addInstance, remove: removeInstance } = useFieldArray({ name: "instances", control });
  const { fields: trainers, append: addTrainer, remove: removeTrainer } = useFieldArray({ name: "trainers", control });
  const { fields: cohorts, append: addCohort, remove: removeCohort } = useFieldArray({ name: "cohorts", control });
  const { fields: registrations, append: addRegistration, remove: removeRegistration } = useFieldArray({ name: "registrations", control });

  const type = watch('type');
  React.useEffect(() => {
    if (type === 'LESSON') {
      setValue('capacity', 1);
    }
    if (type === 'GROUP') {
      setValue('capacity', null);
    }
  }, [type]);

  const trainerOptions = React.useMemo(() => tenantQuery.data?.tenant?.tenantTrainersList.map(trainer => ({
    id: trainer.person?.id || '',
    label: trainer.person?.name || '?',
  })), [tenantQuery]);

  const cohortOptions = React.useMemo(() => tenantQuery.data?.tenant?.skupinies?.nodes?.map(trainer => ({
    id: trainer.id,
    label: trainer.sName || '?',
  })), [tenantQuery]);

  const possibleParticipants = React.useMemo(() => {
    const possibleCouples = (tenantQuery.data?.tenant?.couplesList || []).map((c) => ({
      id: `couple-${c.id}`,
      label: formatLongCoupleName(c),
    }));
    const possiblePersons = (tenantQuery.data?.tenant?.tenantMembershipsList || []).map((p) => ({
      id: `person-${p.id}`,
      label: p.person?.name || '?',
    }));
    return possibleCouples.concat(possiblePersons);
  }, [tenantQuery]);

  const addInstancePlusWeek = React.useCallback(() => {
    const lastInstance = instances[instances.length - 1]!;
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const x = timeRangeToDatetimeRange(lastInstance);
    addInstance(datetimeRangeToTimeRange(add(x.since, 1, 'week'), add(x.until, 1, 'week')));
  }, [instances]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    const result = await create({
      input: {
        info: {
          name: values.name,
          summary: '',
          description: '',
          type: values.type,
          locationText: values.locationText,
          capacity: values.capacity,
          isVisible: values.isVisible,
          isPublic: values.isPublic,
          isLocked: values.isLocked,
          enableNotes: values.enableNotes,
        },
        trainers: values.trainers.map(x => ({
          personId: x.personId,
          lessonsOffered: x.lessonsOffered || null,
        })),
        cohorts: values.cohorts,
        registrations: values.registrations.map(x => {
          const [type, id] = x.id?.split('-') || [];
          return type === 'couple' ? { coupleId: id } : { personId: id };
        }),
        instances: values.instances.map(x => {
          const y = timeRangeToDatetimeRange(x);
          return {
            since: y.since.toISOString(),
            until: y.until.toISOString(),
          };
        }),
      },
    });
    if (result.data?.createEvent?.event?.id) {
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
          { id: 'HOLIDAY', label: 'Prázdniny' },
        ]}
      />

      <TextFieldElement control={control} name="name" label="Název (nepovinný)" />
      <TextFieldElement control={control} name="locationText" label="Místo konání" />
      {type !== 'LESSON' && (
        <TextFieldElement control={control} type="number" name="capacity" label="Maximální počet účastníků (nepovinný)" />
      )}

      {instances.map((instance, index) => (
        <div className="flex gap-2" key={instance.id}>
          <TextFieldElement control={control} name={`instances.${index}.date`} type="date" className="grow" />
          <TextFieldElement control={control} name={`instances.${index}.startTime`} type="time" required />
          <TextFieldElement control={control} name={`instances.${index}.endTime`} type="time" required />
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => removeInstance(index)}
            disabled={instances.length <= 1}
          >
            <X />
          </button>
        </div>
      ))}

      {trainers.map((trainer, index) => (
        <div className="flex gap-2" key={trainer.id}>
          <ComboboxElement
            control={control} name={`trainers.${index}.personId`} placeholder="Vyberte trenéra"
            options={trainerOptions}
          />
          {!['LESSON', 'GROUP'].includes(type) && (
            <TextFieldElement
              control={control}
              type="number"
              name={`trainers.${index}.lessonsOffered`}
              placeholder="Počet lekcí"
              size={1}
            />
          )}
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => removeTrainer(index)}
          >
            <X />
          </button>
        </div>
      ))}

      {cohorts.map((cohort, index) => (
        <div className="flex gap-2" key={cohort.id}>
          <ComboboxElement
            control={control} name={`cohorts.${index}.cohortId`} placeholder="Vyberte skupinu"
            options={cohortOptions}
          />
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => removeCohort(index)}
          >
            <X />
          </button>
        </div>
      ))}

      {registrations.map((registration, index) => (
        <div className="flex gap-2" key={registration.id}>
          <ComboboxElement
            control={control}
            name={`registrations.${index}.id`}
            placeholder="Vyberte účastníka"
            options={possibleParticipants}
          />
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => removeRegistration(index)}
          >
            <X />
          </button>
        </div>
      ))}

      <div className="flex flex-wrap gap-1">
        <button type="button" className={buttonCls({ variant: 'outline' })} onClick={addInstancePlusWeek} >
          <Plus /> Termín+1 týden
        </button>
        <button type="button" className={buttonCls({ variant: 'outline' })} onClick={() => addTrainer({ personId: '', lessonsOffered: 0 })} >
          <Plus /> Trenér
        </button>
        {type !== 'LESSON' && (
          <button type="button" className={buttonCls({ variant: 'outline' })} onClick={() => addCohort({ cohortId: '' })} >
            <Plus /> Skupina
          </button>
        )}
        <button type="button" className={buttonCls({ variant: 'outline' })} onClick={() => addRegistration({ id: '' })} >
          <Plus /> Pár (člověk)
        </button>
      </div>

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
