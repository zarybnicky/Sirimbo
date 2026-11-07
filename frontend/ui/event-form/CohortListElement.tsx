import type { EventFormControl } from '@/ui/event-form/types';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { buttonCls } from '@/ui/style';
import { Plus, X } from 'lucide-react';
import { useFieldArray, useWatch } from 'react-hook-form';

export function CohortListElement({
  control,
  cohorts,
  onAdd,
  onRemove,
}: {
  control: EventFormControl;
  cohorts: { id: string; name: string; isArchived: boolean }[];
  onAdd: (cohortId: string) => void;
  onRemove: (cohortId: string) => void;
}) {
  const type = useWatch({ control, name: 'type' });
  const { fields, append, remove } = useFieldArray({ name: 'cohorts', control });
  const selected = new Set(fields.map((cohort) => cohort.cohortId));

  return (
    <>
      {type !== 'LESSON' && (
        <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
          <b>Tréninkové skupiny</b>

          <ComboboxButton
            align="end"
            side="top"
            trigger={
              <button
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> Skupina
              </button>
            }
            options={cohorts
              .filter((x) => !x.isArchived || selected.has(x.id))
              .map(({ id, name }) => ({ id, label: name, disabled: selected.has(id) }))}
            onChange={(cohortId) => {
              if (cohortId && !fields.some((c) => c.cohortId === cohortId)) {
                onAdd(cohortId);
                append({ cohortId });
              }
            }}
          />
        </div>
      )}

      {fields.map((cohort, index) => (
        <div key={cohort.id} className="flex items-center gap-2">
          <span className="grow">
            {cohorts.find((x) => x.id === cohort.cohortId)?.name}
          </span>
          <button
            type="button"
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={() => {
              onRemove(cohort.cohortId);
              remove(index);
            }}
          >
            <X />
          </button>
        </div>
      ))}
    </>
  );
}
