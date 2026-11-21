import { EventOverlapWarning } from './EventOverlapWarning';
import { SelectionList } from '@/ui/fields/SelectionList';
import { TextFieldElement } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { useController, useFieldArray, useWatch } from 'react-hook-form';
import type { EventFormControl } from '@/ui/event-form/types';

export type EventTrainerOption = {
  id: string;
  label: string;
  hidden: boolean;
  disabled: boolean;
};

export function TrainerListElement({
  control,
  trainers,
  conflicts,
}: {
  control: EventFormControl;
  trainers: EventTrainerOption[];
  conflicts?: Record<string, string[]>;
}) {
  const { fields, append, remove } = useFieldArray({ name: 'trainers', control });
  const type = useWatch({ control, name: 'type' });

  return (
    <SelectionList
      label="Trenéři"
      addLabel="Trenér"
      options={trainers}
      value={fields.map((x) => x.personId)}
      onAdd={(personId) => append({ personId, lessonsOffered: 0 })}
      onRemove={remove}
      renderItem={(id, index) => <>
        <EventOverlapWarning lines={conflicts?.[id]} />
        {!['LESSON', 'GROUP'].includes(type ?? 'LESSON') && (
          <TrainerLessonsField control={control} index={index} />
        )}
      </>}

    />
  );
}

function TrainerLessonsField({
  control,
  index,
}: {
  control: EventFormControl;
  index: number;
}) {
  const { field } = useController({ control, name: `trainers.${index}.lessonsOffered` });
  const offersLessons = field.value !== 0;
  return (
    <div className="flex flex-wrap items-center gap-2">
      <Checkbox
        name={`${field.name}.enabled`}
        label="Požadavky na lekce"
        checked={offersLessons}
        onBlur={field.onBlur}
        onChange={() => field.onChange(offersLessons ? 0 : null)}
      />
      {offersLessons && (
        <TextFieldElement
          control={control}
          type="number"
          name={field.name}
          placeholder="Bez omezení"
          className="w-28"
          min={1}
          size={1}
        />
      )}
    </div>
  );
}
