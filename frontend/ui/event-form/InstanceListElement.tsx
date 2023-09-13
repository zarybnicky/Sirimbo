import { Control, useFieldArray } from "react-hook-form";
import { TypeOf } from "zod";
import { EventForm } from "./types";
import React from "react";
import { buttonCls } from "../style";
import { Plus, X } from 'lucide-react';
import { datetimeRangeToTimeRange, timeRangeToDatetimeRange } from '@/ui/format';
import { add } from 'date-arithmetic';
import { TextFieldElement } from "../fields/text";

export function InstanceListElement({ name, control }: {
  control: Control<TypeOf<typeof EventForm>>;
  name: 'instances';
}) {
  const { fields, append, remove, update } = useFieldArray({ name, control });

  const addInstancePlusWeek = React.useCallback(() => {
    const lastInstance = fields.findLast(x => !!x.date);
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const { since, until } = timeRangeToDatetimeRange(lastInstance);
    append(datetimeRangeToTimeRange(add(since!, 1, 'week'), add(until!, 1, 'week')));
  }, [fields]);

  return (
    <>
      {fields.map((instance, index) => {
        if (instance.date === null) return <React.Fragment key={index} />
        return (
          <div className="flex flex-wrap gap-2" key={instance.id}>
            <TextFieldElement control={control} name={`instances.${index}.date`} type="date" className="grow" />
            <div className="flex gap-2">
              <TextFieldElement control={control} name={`instances.${index}.startTime`} type="time" required />
              <TextFieldElement control={control} name={`instances.${index}.endTime`} type="time" required />
              <button
                type="button"
                className={buttonCls({ size: 'sm', variant: 'outline' })}
                onClick={() => instance.itemId ? update(index, { ...instance, date: null }) : remove(index)}
                disabled={fields.filter(x => !!x.date).length <= 1}
              >
                <X />
              </button>
            </div>
          </div>
        );
      })}

      <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={addInstancePlusWeek} >
        <Plus /> Termín+1 týden
      </button>
    </>
  );
}
