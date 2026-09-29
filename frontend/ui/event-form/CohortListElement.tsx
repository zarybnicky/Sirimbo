import type { EventFormInput, EventFormType } from '@/ui/event-form/types';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';
import { useQuery } from 'urql';
import { EventFormOptionsDocument } from '@/graphql/Event';
import { FormError } from '@/ui/form';

export function CohortListElement({ onAdd, onRemove }: {
  onAdd: (cohortId: string) => void;
  onRemove: (cohortId: string) => void;
}) {
  const { control } = useFormContext<EventFormInput, unknown, EventFormType>();
  const [open, setOpen] = React.useState(false);
  const type = useWatch({ control, name: 'type' });
  const { fields, append, remove } = useFieldArray({ name: 'cohorts', control });

  const [{ data, fetching, error }] = useQuery({
    query: EventFormOptionsDocument,
  });
  const cohorts = data?.tenant?.cohortsList ?? [];

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
                  options={cohorts.filter((x) => !x.isArchived).map((x) => ({ id: x.id, label: x.name }))}
                  onChange={(id) => {
                    if (id && !fields.some((cohort) => cohort.cohortId === id)) {
                      onAdd(id);
                      append({ cohortId: id });
                    }
                    setOpen(false);
                  }}
                />
              </PopoverPrimitive.Content>
            </PopoverPrimitive.Portal>
          </Popover>
        </div>
      )}

      <FormError error={error} />
      {fields.map((cohort, index) => (
        <div key={cohort.id} className="flex items-center gap-2">
          <span className="grow">
            {cohorts.find((x) => x.id === cohort.cohortId)?.name ??
              (fetching ? 'Načítám…' : cohort.cohortId)}
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
