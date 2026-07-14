import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { type Control, useFieldArray, useWatch } from 'react-hook-form';
import { EventForm } from '@/ui/event-form/types';
import { z } from 'zod';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import { keyIsNonNull } from '@/lib/truthyFilter';

export function InstanceTrainerListElement({
  name,
  control,
  mode,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  name: 'trainers' | `instances.${number}.trainers`;
  mode: 'add' | 'edit';
}) {
  const auth = useAuth();
  const [open, setOpen] = React.useState(false);
  const { fields, append, remove, update, replace } = useFieldArray({ name, control });
  const type = useWatch({ control, name: 'type' });
  const value = useWatch({ control, name });

  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const trainers = React.useMemo(
    () =>
      (tenant?.tenant?.tenantTrainersList || [])
        .filter(keyIsNonNull('person'))
        .map(({ person: { id, name } }) => ({ id, label: name })),
    [tenant],
  );

  const enabledTrainers = React.useMemo(
    () => (auth.isAdmin ? trainers : trainers.filter((x) => auth.isMyPerson(x.id))),
    [trainers, auth],
  );

  React.useEffect(() => {
    const firstTrainer = enabledTrainers.at(0);
    if (
      mode === 'add' &&
      fields.length === 0 &&
      enabledTrainers.length === 1 &&
      firstTrainer
    ) {
      replace([{ itemId: null, personId: firstTrainer.id, lessonsOffered: 0 }]);
    }
  }, [fields.length, mode, replace, enabledTrainers]);

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <div>
          <b>Trenéři</b>
        </div>

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
                  if (id) append({ itemId: null, personId: id, lessonsOffered: 0 });
                  setOpen(false);
                }}
                options={enabledTrainers}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      {fields.map((trainer, trainerIndex) => {
        const currentTrainer = value?.[trainerIndex] ?? trainer;
        const lessonsOffered = currentTrainer.lessonsOffered as number | null | undefined;
        const offersLessons = lessonsOffered !== 0;

        return !trainer.personId ? (
          <React.Fragment key={trainer.id} />
        ) : (
          <div className="flex flex-wrap items-center gap-2" key={trainer.id}>
            <div className="grow">
              {trainers.find((x) => x.id === trainer.personId)?.label}
            </div>
            {!['LESSON', 'GROUP'].includes(type) && (
              <div className="flex flex-wrap items-center gap-2">
                <label className="flex min-h-9 items-center gap-2 whitespace-nowrap text-sm text-neutral-12">
                  <input
                    type="checkbox"
                    className="focus:ring-accent-9 size-4 bg-accent-2 text-accent-10 border-accent-9 border-2 rounded"
                    checked={offersLessons}
                    onChange={(event) =>
                      update(trainerIndex, {
                        itemId: currentTrainer.itemId,
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
                    name={`${name}.${trainerIndex}.lessonsOffered`}
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
              onClick={() =>
                trainer.itemId
                  ? update(trainerIndex, { ...trainer, personId: null })
                  : remove(trainerIndex)
              }
            >
              <X />
            </button>
          </div>
        );
      })}
    </>
  );
}
