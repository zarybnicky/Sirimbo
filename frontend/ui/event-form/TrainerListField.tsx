import { Control, useFieldArray, useWatch } from "react-hook-form";
import { TypeOf } from "zod";
import { EventForm } from "./types";
import React from "react";
import { useQuery } from "urql";
import { CurrentTenantDocument } from "@/graphql/Tenant";
import { buttonCls } from "../style";
import { TextFieldElement } from "../fields/text";
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import { ComboboxSearchArea } from "../Combobox";
import { useAuth } from "../use-auth";

/* export type FieldArrayPathByValue<TFieldValues extends FieldValues, TValue> = {
*   [Key in ArrayPath<TFieldValues>]: FieldArrayPathValue<TFieldValues, Key> extends TValue ? Key : never;
* }[FieldArrayPath<TFieldValues>]; */

export function TrainerListElement({ name, control }: {
  control: Control<TypeOf<typeof EventForm>>;
  name: 'trainers';
}) {
  const [open, setOpen] = React.useState(false);

  const {perms} = useAuth();
  const [tenantQuery] = useQuery({ query: CurrentTenantDocument });
  const trainerOptions = React.useMemo(() => (tenantQuery.data?.tenant?.tenantTrainersList || []).filter(x => x.active).map(trainer => ({
    id: trainer.person?.id || '',
    label: trainer.person?.name || '?',
  })), [tenantQuery]);

  const enabledTrainerOptions = !perms.isAdmin ? trainerOptions.filter(x => perms.isCurrentPerson(x.id)) : trainerOptions;

  const { fields, append, remove, update } = useFieldArray({ name, control });
  const type = useWatch({ control, name: 'type' });

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <div><b>Trenéři</b></div>

        <Popover open={open} onOpenChange={setOpen}>
          <PopoverTrigger asChild>
            <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
              <Plus /> Trenér
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content className="z-40" align="end" side='top' sideOffset={5}>
              <ComboboxSearchArea
                value={null}
                onChange={(id) => {
                  if (id) append({ itemId: null, personId: id, lessonsOffered: 0 })
                  setOpen(false);
                }}
                options={enabledTrainerOptions}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      {fields.map((trainer, index) => !trainer.personId ? <React.Fragment key={index} /> : (
        <div className="flex items-baseline gap-2" key={trainer.id}>
          <div className="grow">
            {trainerOptions.find(x => x.id === trainer.personId)?.label}
          </div>
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
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={() => trainer.itemId ? update(index, { ...trainer, personId: null }) : remove(index)}
          >
            <X />
          </button>
        </div>
      ))}
    </>
  );
}
