import { ComboboxSearchArea } from "@/ui/fields/Combobox";
import { TextFieldElement } from "@/ui/fields/text";
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from "@/ui/style";
import { useAuth } from "@/ui/use-auth";
import { useTenant } from "@/ui/useTenant";
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from "react";
import { type Control, useFieldArray, useWatch } from "react-hook-form";
import type { EventFormType } from "@/ui/event-form/types";

export function TrainerListElement({ name, control }: {
  control: Control<EventFormType>;
  name: 'trainers';
}) {
  const [open, setOpen] = React.useState(false);
  const { fields, append, remove, update, replace } = useFieldArray({ name, control });
  const type = useWatch({ control, name: 'type' });


  const { data: tenant } = useTenant();
  const trainerOptions = React.useMemo(
    () => (tenant?.tenantTrainersList || []).filter(x => x.active).map(trainer => ({
      id: trainer.person?.id || '',
      label: trainer.person?.name || '?',
    })),
    [tenant],
  );

  const auth = useAuth();
  const enabledTrainerOptions = React.useMemo(
    () => auth.isAdmin ? trainerOptions : trainerOptions.filter(x => auth.personIds.some(id => id === x.id)),
    [trainerOptions, auth],
  );

  React.useEffect(() => {
    const firstTrainer = enabledTrainerOptions.find(() => true);
    if (enabledTrainerOptions.length  === 1 && firstTrainer) {
      replace([{ itemId: null, personId: firstTrainer.id, lessonsOffered: 0 }]);
    }
  }, [replace, enabledTrainerOptions]);

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

      {fields.map((trainer, index) => !trainer.personId ? <React.Fragment key={trainer.id} /> : (
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
