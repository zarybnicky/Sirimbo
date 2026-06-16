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

export function TrainerListElement({
  name,
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  name: 'trainers';
}) {
  const [open, setOpen] = React.useState(false);
  const { fields, append, remove, update, replace } = useFieldArray({ name, control });
  const type = useWatch({ control, name: 'type' });
  const trainers = useWatch({ control, name });

  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const trainerOptions = React.useMemo(
    () =>
      (tenant?.tenant?.tenantTrainersList || [])
        .filter((x) => x.status === 'ACTIVE')
        .map((trainer) => ({
          id: trainer.person?.id || '',
          label: trainer.person?.name || '?',
        })),
    [tenant],
  );

  const auth = useAuth();
  const enabledTrainerOptions = React.useMemo(
    () =>
      auth.isAdmin ? trainerOptions : trainerOptions.filter((x) => auth.isMyPerson(x.id)),
    [trainerOptions, auth],
  );

  React.useEffect(() => {
    const firstTrainer = enabledTrainerOptions.find(() => true);
    if (enabledTrainerOptions.length === 1 && firstTrainer) {
      replace([{ itemId: null, personId: firstTrainer.id, lessonsOffered: 0 }]);
    }
  }, [replace, enabledTrainerOptions]);

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
                options={enabledTrainerOptions}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      {fields.map((trainer, index) => {
        const currentTrainer = trainers?.[index] ?? trainer;
        const lessonsOffered = currentTrainer.lessonsOffered as number | null | undefined;
        const offersLessons = lessonsOffered !== 0;

        return !trainer.personId ? (
          <React.Fragment key={trainer.id} />
        ) : (
          <div className="flex flex-wrap items-center gap-2" key={trainer.id}>
            <div className="min-w-40 grow">
              {trainerOptions.find((x) => x.id === trainer.personId)?.label}
            </div>
            {!['LESSON', 'GROUP'].includes(type) && (
              <div className="flex flex-wrap items-center gap-2">
                <label className="flex min-h-9 items-center gap-2 whitespace-nowrap text-sm text-neutral-12">
                  <input
                    type="checkbox"
                    className="focus:ring-accent-9 size-4 bg-accent-2 text-accent-10 border-accent-9 border-2 rounded"
                    checked={offersLessons}
                    onChange={(event) =>
                      update(index, {
                        itemId: currentTrainer.itemId,
                        personId: currentTrainer.personId,
                        lessonsOffered: event.currentTarget.checked ? null : 0,
                      })
                    }
                  />
                  Povolit nabídku lekcí
                </label>
                {offersLessons && (
                  <TextFieldElement
                    control={control}
                    type="number"
                    name={`trainers.${index}.lessonsOffered`}
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
                  ? update(index, { ...trainer, personId: null })
                  : remove(index)
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
