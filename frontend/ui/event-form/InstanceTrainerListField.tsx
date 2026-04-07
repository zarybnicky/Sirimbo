import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { Popover, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { type Control, useFieldArray } from 'react-hook-form';
import { EventForm } from '@/ui/event-form/types';
import { z } from 'zod';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';

export function InstanceTrainerListElement({
  index,
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  index: number;
}) {
  const [open, setOpen] = React.useState(false);
  const { fields, append, remove, update, replace } = useFieldArray({
    name: `instances.${index}.trainers`,
    control,
  });

  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const trainerOptions = React.useMemo(
    () =>
      (tenant?.tenant?.tenantTrainersList || []).map((trainer) => ({
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
      replace([{ itemId: null, personId: firstTrainer.id }]);
    }
  }, [replace, enabledTrainerOptions]);

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <div>
          <b>Trenéři termínu</b>
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
                  if (id) append({ itemId: null, personId: id });
                  setOpen(false);
                }}
                options={enabledTrainerOptions}
              />
            </PopoverPrimitive.Content>
          </PopoverPrimitive.Portal>
        </Popover>
      </div>

      {fields.map((trainer, index) =>
        !trainer.personId ? (
          <React.Fragment key={trainer.id} />
        ) : (
          <div className="flex items-baseline gap-2" key={trainer.id}>
            <div className="grow">
              {trainerOptions.find((x) => x.id === trainer.personId)?.label}
            </div>
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
        ),
      )}
    </>
  );
}
