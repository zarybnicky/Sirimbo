import { Control, useFieldArray, useWatch } from "react-hook-form";
import { TypeOf } from "zod";
import { EventForm } from "./types";
import React from "react";
import { useQuery } from "urql";
import { CurrentTenantDocument } from "@/graphql/Tenant";
import { buttonCls } from "../style";
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import { ComboboxSearchArea } from "../Combobox";

export function CohortListElement({ name, control }: {
  control: Control<TypeOf<typeof EventForm>>;
  name: 'cohorts';
}) {
  const [open, setOpen] = React.useState(false);
  const type = useWatch({ control, name: 'type' });
  const { fields, append, remove, update } = useFieldArray({ name, control });

  const [tenantQuery] = useQuery({ query: CurrentTenantDocument });
  const cohortOptions = React.useMemo(() => (tenantQuery.data?.tenant?.skupinies?.nodes || [])?.map(trainer => ({
    id: trainer.id,
    label: trainer.sName || '?',
  })), [tenantQuery]);

  return (
    <>
      {fields.map((cohort, index) => {
        if (!cohort.cohortId) return <React.Fragment key={index} />
        return (
          <div className="flex gap-2" key={cohort.id}>
            {cohortOptions.find(x => x.id === cohort.cohortId)?.label}
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => cohort.itemId ? update(index, { ...cohort, cohortId: null }) : remove(index)}
            >
              <X />
            </button>
          </div>
        );
      })}

      {type !== 'LESSON' && (
        <Popover open={open} onOpenChange={setOpen}>
          <PopoverTrigger asChild>
            <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })}>
              <Plus /> Skupina
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content className="z-40 PopoverContent" align="start" side='top' sideOffset={5}>
              <ComboboxSearchArea
                value={null}
                onChange={(id) => {
                  if (id) append({ cohortId: id })
                  setOpen(false)
                }}
                options={cohortOptions}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      )}
    </>
  );
}
