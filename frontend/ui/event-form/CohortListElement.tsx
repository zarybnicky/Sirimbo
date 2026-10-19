import type { EventFormInput, EventFormType } from '@/ui/event-form/types';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';

export function CohortListElement({ cohorts, onAdd, onRemove }: {
  cohorts: { id: string; name: string; isArchived: boolean }[];
  onAdd: (cohortId: string) => void;
  onRemove: (cohortId: string) => void;
}) {
  const { control } = useFormContext<EventFormInput, unknown, EventFormType>();
  const [open, setOpen] = React.useState(false);
  const type = useWatch({ control, name: 'type' });
  const { fields, append, remove } = useFieldArray({ name: 'cohorts', control });

  return (
    <>
      {type !== 'LESSON' && (
        <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
          <b>Tréninkové skupiny</b>

          <Popover open={open} onOpenChange={setOpen}>
            <PopoverTrigger asChild>
              <button
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> Skupina
              </button>
            </PopoverTrigger>
            <PopoverPrimitive.Portal>
              <PopoverPrimitive.Content
                className="z-40 max-h-(--radix-popover-content-available-height)"
                align="end"
                side="top"
                sideOffset={5}
              >
                <ComboboxSearchArea
                  options={cohorts
                    .filter((x) => !x.isArchived && !fields.some((y) => y.cohortId === x.id))
                    .map(({ id, name }) => ({ id, label: name }))}
                  onChange={(cohortId) => {
                    if (cohortId && !fields.some((c) => c.cohortId === cohortId)) {
                      onAdd(cohortId);
                      append({ cohortId });
                    }
                    setOpen(false);
                  }}
                />
              </PopoverPrimitive.Content>
            </PopoverPrimitive.Portal>
          </Popover>
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
            aria-label="Odebrat skupinu"
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
