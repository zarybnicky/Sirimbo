import type { EventFormType } from '@/ui/event-form/types';
import { TextFieldElement } from '@/ui/fields/text';
import { datetimeRangeToTimeRange, timeRangeToDatetimeRange } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { add } from 'date-arithmetic';
import { Plus, X } from 'lucide-react';
import React from 'react';
import { type Control, useFieldArray } from 'react-hook-form';

export function InstanceListElement({
  name,
  control,
}: {
  control: Control<EventFormType>;
  name: 'instances';
}) {
  const { fields, append, remove, update } = useFieldArray({ name, control });

  const addInstancePlusWeek = React.useCallback(() => {
    const lastInstance = (fields || []).findLast((x) => !!x.date);
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const { since, until } = timeRangeToDatetimeRange(lastInstance);
    append({
      ...(datetimeRangeToTimeRange(add(since!, 1, 'week'), add(until!, 1, 'week'))),
      isCancelled: false,
    });
  }, [append, fields]);

  const addInstancePlus2Weeks = React.useCallback(() => {
    const lastInstance = (fields || []).findLast((x) => !!x.date);
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const { since, until } = timeRangeToDatetimeRange(lastInstance);
    append({
      ...(datetimeRangeToTimeRange(add(since!, 2, 'week'), add(until!, 2, 'week'))),
      isCancelled: false,
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

      {fields.map((instance, index) =>
        !instance.date ? (
          <React.Fragment key={index} />
        ) : (
          <div className="flex flex-wrap gap-2" key={instance.id}>
            <TextFieldElement
              control={control}
              name={`instances.${index}.date`}
              type="date"
              className="grow"
            />
            <div className="flex gap-2 items-baseline">
              <TextFieldElement
                control={control}
                name={`instances.${index}.startTime`}
                type="time"
                required
              />
              <TextFieldElement
                control={control}
                name={`instances.${index}.endTime`}
                type="time"
                required
              />
              <button
                type="button"
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                disabled={fields.filter((x) => !!x.date).length <= 1}
                onClick={() =>
                  instance.itemId
                    ? update(index, { ...instance, date: null })
                    : remove(index)
                }
              >
                <X />
              </button>
            </div>
          </div>
        ),
      )}
    </>
  );
}
