import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls, checkboxInputCls } from '@/ui/style';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { useFieldArray, useWatch } from 'react-hook-form';
import type { EventFormControl } from '@/ui/event-form/types';

export type EventTrainerOption = { id: string; label: string; enabled: boolean };

export function TrainerListElement({ control, trainers }: {
  control: EventFormControl;
  trainers: EventTrainerOption[];
}) {
  const [open, setOpen] = React.useState(false);
  const { fields, append, remove, update } = useFieldArray({ name: 'trainers', control });
  const [type, value] = useWatch({ control, name: ['type', 'trainers'] });

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <b>Trenéři</b>

        <Popover open={open} onOpenChange={setOpen}>
          <PopoverTrigger asChild>
            <button
              type="button"
              className={buttonCls({ size: 'xs', variant: 'outline' })}
            >
              <Plus /> Trenér
            </button>
          </PopoverTrigger>
          <PopoverPrimitive.Portal>
            <PopoverPrimitive.Content
              className="z-40"
              align="end"
              side="top"
              sideOffset={5}
            >
              <ComboboxSearchArea
                onChange={(id) => {
                  if (id && !fields.some((trainer) => trainer.personId === id)) {
                    append({ personId: id, lessonsOffered: 0 });
                  }
                  setOpen(false);
                }}
                options={trainers.filter((x) => x.enabled && !fields.some((y) => y.personId === x.id))}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      {fields.map((trainer, trainerIndex) => {
        const currentTrainer = value?.[trainerIndex] ?? trainer;
        const offersLessons = currentTrainer.lessonsOffered !== 0;

        return (
          <div className="flex flex-wrap items-center gap-2" key={trainer.id}>
            <div className="grow">
              {trainers.find((x) => x.id === trainer.personId)?.label}
            </div>
            {!['LESSON', 'GROUP'].includes(type ?? 'LESSON') && (
              <div className="flex flex-wrap items-center gap-2">
                <label className="flex min-h-9 items-center gap-2 whitespace-nowrap text-sm text-neutral-12">
                  <input
                    type="checkbox"
                    className={checkboxInputCls()}
                    checked={offersLessons}
                    onChange={(event) =>
                      update(trainerIndex, {
                        personId: currentTrainer.personId,
                        lessonsOffered: event.currentTarget.checked ? null : 0,
                      })
                    }
                  />
                  Požadavky na lekce
                </label>
                {offersLessons && (
                  <TextFieldElement
                    control={control}
                    type="number"
                    name={`trainers.${trainerIndex}.lessonsOffered`}
                    placeholder="Bez omezení"
                    aria-label="Limit lekcí"
                    className="w-28"
                    min={1}
                    size={1}
                  />
                )}
              </div>
            )}
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => remove(trainerIndex)}
            >
              <X />
            </button>
          </div>
        );
      })}
    </>
  );
}
