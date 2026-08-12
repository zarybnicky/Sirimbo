import type { EventFormControl } from '@/ui/event-form/types';
import { buttonCls } from '@/ui/style';
import { add } from 'date-arithmetic';
import { Plus, X } from 'lucide-react';
import { useFieldArray, useWatch } from 'react-hook-form';
import { DateTimeRangeField } from './DateTimeRangeField';

export function InstanceListElement({ control }: { control: EventFormControl }) {
  const { fields, append, remove } = useFieldArray({ name: 'instances', control });
  const instances = useWatch({ control, name: 'instances' });

  const addInstance = (weeks: 1 | 2) => {
    const previous = instances.at(-1);
    const since = previous ? new Date(previous.since) : new Date();
    const until = previous ? new Date(previous.until) : add(since, 45, 'minutes');
    append({
      itemId: null,
      since: add(since, weeks, 'week').toISOString(),
      until: add(until, weeks, 'week').toISOString(),
      isCancelled: false,
    });
  };

  return (
    <div className="space-y-2">
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <b>Termíny</b>
        <div className="flex flex-wrap gap-2">
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={() => addInstance(1)}
          >
            <Plus /> 1&nbsp;týden
          </button>
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={() => addInstance(2)}
          >
            <Plus /> 2&nbsp;týdny
          </button>
        </div>
      </div>

      {fields.map((instance, index) => (
        <div className="flex flex-wrap items-start gap-2" key={instance.id}>
          <DateTimeRangeField
            control={control}
            nameSince={`instances.${index}.since`}
            nameUntil={`instances.${index}.until`}
          />
          {!instance.itemId && fields.length > 1 && (
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              aria-label={`Odstranit termín ${index + 1}`}
              onClick={() => remove(index)}
            >
              <X />
            </button>
          )}
        </div>
      ))}
    </div>
  );
}
