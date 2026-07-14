import React from 'react';
import { TextField } from '@/ui/fields/text';
import { PeriodPreset } from './periods';
import { SelectField, type SelectOption } from '@/ui/fields/select';

type PeriodSelectorProps = {
  preset: PeriodPreset;
  onPresetChange: (preset: PeriodPreset) => void;
  date: Date;
  onDateChange: (value: Date) => void;
  since: Date | null;
  onSinceChange: (value: Date | null) => void;
  until: Date | null;
  onUntilChange: (value: Date | null) => void;
};

const periodOptions = Object.entries({
  schoolyear: 'Školní rok',
  semester: 'Pololetí',
  quarter: 'Čtvrtletí',
  month: 'Měsíc',
  custom: 'Vlastní interval',
}).map(([value, label]) => ({
  value: value as PeriodPreset,
  label,
})) satisfies SelectOption<PeriodPreset>[];

export function PeriodSelector({
  preset,
  onPresetChange,
  date,
  onDateChange,
  since,
  onSinceChange,
  until,
  onUntilChange,
}: PeriodSelectorProps) {
  return (
    <>
      <SelectField
        label="Období"
        value={preset}
        onChange={onPresetChange}
        options={periodOptions}
      />

      {preset === 'custom' ? (
        <>
          <TextField
            label="Od"
            name="scoreboard-since"
            type="date"
            value={since?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onSinceChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
          <TextField
            label="Do"
            name="scoreboard-until"
            type="date"
            value={until?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onUntilChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
        </>
      ) : (
        <TextField
          label="K datu"
          name="scoreboard-reference-date"
          type="date"
          value={date.toISOString().slice(0, 10)}
          onChange={(event) =>
            onDateChange(
              event.currentTarget.value
                ? new Date(event.currentTarget.value)
                : new Date(),
            )
          }
        />
      )}
    </>
  );
}
