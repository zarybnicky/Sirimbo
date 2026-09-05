import { cn } from '@/lib/cn';
import { InputGroup } from '@/ui/fields/text';
import { FieldHelper } from '@/ui/form';
import { inputCls } from '@/ui/style';
import {
  addHours,
  addMilliseconds,
  differenceInMilliseconds,
  format,
  isSameDay,
  isValid,
  parse,
} from 'date-fns';
import { useController, useWatch } from 'react-hook-form';
import type { EventFormControl } from '@/ui/event-form/types';

function parseLocalDateTime(date: string, time: string) {
  const value = parse(`${date} ${time}`, 'yyyy-MM-dd HH:mm', new Date());
  return isValid(value) ? value : null;
}

export function DateTimeRangeField({
  control,
  nameSince,
  nameUntil,
  className,
}: {
  control: EventFormControl;
  nameSince: `instances.${number}.since`;
  nameUntil: `instances.${number}.until`;
  className?: string;
}) {
  const since = useController({ control, name: nameSince });
  const until = useController({ control, name: nameUntil });
  const type = useWatch({ control, name: 'type' });

  const sinceDate = since.field.value ? new Date(since.field.value) : new Date();
  const untilDate = until.field.value
    ? new Date(until.field.value)
    : addHours(sinceDate, 1);
  const startDate = format(sinceDate, 'yyyy-MM-dd');
  const startTime = format(sinceDate, 'HH:mm');
  const endDate = format(untilDate, 'yyyy-MM-dd');
  const endTime = format(untilDate, 'HH:mm');
  const showEndDate = type === 'CAMP' || !isSameDay(sinceDate, untilDate);
  const rangeError =
    untilDate < sinceDate
      ? { type: 'validate', message: 'Konec události je dřiv než začátek' }
      : undefined;

  const setSince = (date: string, time: string) => {
    const nextSince = parseLocalDateTime(date, time);
    if (!nextSince) return;

    const duration = Math.max(0, differenceInMilliseconds(untilDate, sinceDate));
    since.field.onChange(nextSince.toISOString());
    until.field.onChange(addMilliseconds(nextSince, duration).toISOString());
  };

  const setUntil = (date: string, time: string) => {
    const nextUntil = parseLocalDateTime(date, time);
    if (nextUntil) until.field.onChange(nextUntil.toISOString());
  };

  return (
    <div className={cn('min-w-0 flex-1', className)}>
      <div className={cn('flex gap-2', type === 'CAMP' ? 'flex-col' : 'flex-wrap')}>
        {showEndDate ? (
          <InputGroup className="w-full sm:w-auto">
            <input
              type="date"
              value={startDate}
              aria-label={type === 'CAMP' ? 'Začátek (datum)' : 'Datum'}
              onChange={(event) => setSince(event.currentTarget.value, startTime)}
              className={inputCls({ className: 'min-w-0 grow basis-36' })}
            />
            <input
              type="time"
              step={60}
              value={startTime}
              required
              aria-label="Začátek"
              onChange={(event) => setSince(startDate, event.currentTarget.value)}
              className={inputCls({ className: 'min-w-0 grow basis-24' })}
            />
          </InputGroup>
        ) : (
          <>
            <input
              type="date"
              value={startDate}
              aria-label="Datum"
              onChange={(event) => setSince(event.currentTarget.value, startTime)}
              className={inputCls({ className: 'w-40 shadow-xs' })}
            />
            <InputGroup className="w-56 max-w-full">
              <input
                type="time"
                step={60}
                value={startTime}
                required
                aria-label="Začátek"
                onChange={(event) => setSince(startDate, event.currentTarget.value)}
                className={inputCls({ className: 'min-w-0' })}
              />
              <input
                type="time"
                step={60}
                value={endTime}
                required
                aria-label="Konec"
                aria-invalid={rangeError ? true : undefined}
                onChange={(event) => setUntil(startDate, event.currentTarget.value)}
                className={inputCls({ className: 'min-w-0' })}
              />
            </InputGroup>
          </>
        )}

        {showEndDate && (
          <InputGroup className="w-full sm:w-auto">
            <input
              type="date"
              value={endDate}
              required={type === 'CAMP'}
              aria-label="Konec (datum)"
              aria-invalid={rangeError ? true : undefined}
              onChange={(event) => setUntil(event.currentTarget.value, endTime)}
              className={inputCls({ className: 'min-w-0 grow basis-36' })}
            />
            <input
              type="time"
              step={60}
              value={endTime}
              required
              aria-label="Konec"
              aria-invalid={rangeError ? true : undefined}
              onChange={(event) => setUntil(endDate, event.currentTarget.value)}
              className={inputCls({ className: 'min-w-0 grow basis-24' })}
            />
          </InputGroup>
        )}
      </div>
      <FieldHelper error={rangeError} />
    </div>
  );
}
