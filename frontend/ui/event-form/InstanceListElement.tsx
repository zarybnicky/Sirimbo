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
  const { fields, append, remove } = useFieldArray({ name, control });

  const addInstancePlusWeek = React.useCallback(() => {
    const lastInstance = fields[fields.length - 1]!;
    if (!lastInstance) return datetimeRangeToTimeRange(new Date(), new Date());
    const x = timeRangeToDatetimeRange(lastInstance);
    append(datetimeRangeToTimeRange(add(x.since, 1, 'week'), add(x.until, 1, 'week')));
  }, [fields]);

  return (
    <>
      {fields.map((instance, index) => (
        <div className="flex flex-wrap gap-2" key={instance.id}>
          <TextFieldElement control={control} name={`instances.${index}.date`} type="date" className="grow" />
          <div className="flex gap-2">
            <TextFieldElement control={control} name={`instances.${index}.startTime`} type="time" required />
            <TextFieldElement control={control} name={`instances.${index}.endTime`} type="time" required />
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => remove(index)}
              disabled={fields.length <= 1}
            >
              <X />
            </button>
          </div>
        </div>
      ))}

      <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={addInstancePlusWeek} >
        <Plus /> Termín+1 týden
      </button>
    </>
  );
}
