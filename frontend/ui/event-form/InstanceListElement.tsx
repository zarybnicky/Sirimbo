import { cn } from '@/ui/cn';
import { EventForm } from '@/ui/event-form/types';
import { TextFieldElement } from '@/ui/fields/text';
import { datetimeRangeToTimeRange, timeRangeToDatetimeRange } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { add } from 'date-arithmetic';
import { ChevronRightIcon, CircleAlertIcon, Plus, X } from 'lucide-react';
import React from 'react';
import { type Control, useFieldArray, useWatch } from 'react-hook-form';
import * as Collapsible from '@radix-ui/react-collapsible';
import { InstanceTrainerListElement } from './InstanceTrainerListField';
import { z } from 'zod';

export function InstanceListElement({
  name,
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
  name: 'instances';
}) {
  const { fields, append, remove, update } = useFieldArray({ name, control });
  const type = useWatch({ control, name: 'type' }) ?? 'LESSON';
  const isCamp = type === 'CAMP';

  const addInstancePlusWeek = React.useCallback(() => {
    const lastInstance = (fields || []).findLast((x) => !!x.date);
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const { date } = lastInstance;
    if (!date) return datetimeRangeToTimeRange(new Date(), new Date());
    const { since, until } = timeRangeToDatetimeRange(date, lastInstance);
    append({
      ...datetimeRangeToTimeRange(add(since, 1, 'week'), add(until, 1, 'week')),
      isCancelled: false,
      trainers: [],
    });
  }, [append, fields]);

  const addInstancePlus2Weeks = React.useCallback(() => {
    const lastInstance = (fields || []).findLast((x) => !!x.date);
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const { date } = lastInstance;
    if (!date) return datetimeRangeToTimeRange(new Date(), new Date());
    const { since, until } = timeRangeToDatetimeRange(date, lastInstance);
    append({
      ...datetimeRangeToTimeRange(add(since, 2, 'week'), add(until, 2, 'week')),
      isCancelled: false,
      trainers: [],
    });
  }, [append, fields]);

  return (
    <>
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <div>
          <b>Termíny</b>
        </div>

        <div className="flex flex-wrap gap-2">
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={addInstancePlusWeek}
          >
            <Plus /> 1&nbsp;týden
          </button>

          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={addInstancePlus2Weeks}
          >
            <Plus /> 2&nbsp;týdny
          </button>
        </div>
      </div>

      {fields.map((instance, index) => {
        if (!instance.date) return <React.Fragment key={instance.id} />;
        return (
          <Collapsible.Root asChild key={instance.id || index}>
            <div className="flex flex-col gap-2" key={instance.id || instance.date}>
              <div className="flex flex-wrap items-start gap-2">
                <div className="relative pt-2">
                  {instance.trainers && instance.trainers.length > 0 && (
                    <div className="absolute -left-4 top-3 -mt-0.5">
                      <CircleAlertIcon className="size-4" />
                    </div>
                  )}
                  <Collapsible.Trigger asChild>
                    <button className="relative data-[state=open]:rotate-90">
                      <ChevronRightIcon />
                    </button>
                  </Collapsible.Trigger>
                </div>

                <div
                  className={cn(
                    'flex min-w-0 flex-1 gap-2',
                    isCamp ? 'flex-col' : 'flex-wrap items-baseline',
                  )}
                >
                  <div className="flex flex-wrap items-baseline gap-2">
                    <TextFieldElement
                      control={control}
                      name={`instances.${index}.date`}
                      type="date"
                      className="grow"
                      aria-label={isCamp ? 'Začátek (datum)' : undefined}
                    />
                    <TextFieldElement
                      control={control}
                      name={`instances.${index}.startTime`}
                      type="time"
                      required
                      aria-label="Začátek"
                    />
                    {!isCamp && (
                      <TextFieldElement
                        control={control}
                        name={`instances.${index}.endTime`}
                        type="time"
                        required
                        aria-label="Konec"
                      />
                    )}
                  </div>

                  {isCamp && (
                    <div className="flex flex-wrap items-baseline gap-2">
                      <TextFieldElement
                        control={control}
                        name={`instances.${index}.endDate`}
                        type="date"
                        className="grow"
                        required
                        aria-label="Konec (datum)"
                      />
                      <TextFieldElement
                        control={control}
                        name={`instances.${index}.endTime`}
                        type="time"
                        required
                        aria-label="Konec"
                      />
                    </div>
                  )}
                </div>

                <button
                  type="button"
                  className={cn(
                    buttonCls({ size: 'sm', variant: 'outline' }),
                    'self-start',
                  )}
                  disabled={fields.filter((x) => !!x.date).length <= 1}
                  onClick={() =>
                    instance.itemId
                      ? update(index, { ...instance, date: null, endDate: null })
                      : remove(index)
                  }
                >
                  <X />
                </button>
              </div>

              <Collapsible.Content
                asChild
                forceMount
                className="[&[data-state=closed]>div]:hidden"
              >
                <div className="ml-4 w-full items-baseline gap-2">
                  <InstanceTrainerListElement control={control} index={index} />
                </div>
              </Collapsible.Content>
            </div>
          </Collapsible.Root>
        );
      })}
    </>
  );
}
