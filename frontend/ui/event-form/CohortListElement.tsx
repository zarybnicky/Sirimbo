import type { EventFormType } from "@/ui/event-form/types";
import { ComboboxSearchArea } from "@/ui/fields/Combobox";
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from "@/ui/style";
import { useCohorts } from "@/ui/useCohorts";
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from "react";
import { type Control, useFieldArray, useWatch } from "react-hook-form";

export function CohortListElement({ name, control }: {
  control: Control<EventFormType>;
  name: 'cohorts';
}) {
  const [open, setOpen] = React.useState(false);
  const type = useWatch({ control, name: 'type' });
  const { fields, append, remove, update } = useFieldArray({ name, control });

  const { data: cohorts } = useCohorts();
  const cohortOptions = React.useMemo(() => cohorts.map(x => ({
    id: x.id,
    label: x.name,
  })), [cohorts]);

  return (
    <>
      {type !== 'LESSON' && (
        <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
          <div><b>Tréninkové skupiny</b></div>

          <Popover open={open} onOpenChange={setOpen}>
            <PopoverTrigger asChild>
              <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
                <Plus /> Skupina
              </button>
            </PopoverTrigger>
            <PopoverPrimitive.Portal>
              <PopoverPrimitive.Content className="z-40 PopoverContent" align="end" side='top' sideOffset={5}>
                <ComboboxSearchArea
                  options={cohortOptions}
                  onChange={(id) => {
                    if (id) append({ cohortId: id })
                    setOpen(false)
                  }}
                />
              </PopoverPrimitive.Content>
            </PopoverPrimitive.Portal>
          </Popover>
        </div>
      )}

      {fields.map((cohort, index) => !cohort.cohortId ? <React.Fragment key={index} /> : (
        <div className="flex gap-2" key={cohort.id}>
          <div className="grow">
            {cohortOptions.find(x => x.id === cohort.cohortId)?.label}
          </div>
          <button
            type="button"
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={() => cohort.itemId ? update(index, { ...cohort, cohortId: null }) : remove(index)}
          >
            <X />
          </button>
        </div>
      ))}
    </>
  );
}
