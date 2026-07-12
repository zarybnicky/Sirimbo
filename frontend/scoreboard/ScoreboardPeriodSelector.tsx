import React from 'react';
import { TextField } from '@/ui/fields/text';
import { PeriodPreset, periodLabels } from './periods';
import { SelectField, type SelectOption } from '@/ui/fields/select';

type ScoreboardPeriodSelectorProps = {
  preset: PeriodPreset;
  onPresetChange: (preset: PeriodPreset) => void;
  referenceDate: Date;
  onReferenceDateChange: (value: Date) => void;
  since: Date | null;
  onSinceChange: (value: Date | null) => void;
  until: Date | null;
  onUntilChange: (value: Date | null) => void;
};

export function ScoreboardPeriodSelector({
  preset,
  onPresetChange,
  referenceDate,
  onReferenceDateChange,
  since,
  onSinceChange,
  until,
  onUntilChange,
}: ScoreboardPeriodSelectorProps) {
  const periodOptions = React.useMemo(
    () =>
      Object.entries(periodLabels).map(([value, label]) => ({
        value: value as PeriodPreset,
        label,
      })) satisfies SelectOption<PeriodPreset>[],
    [],
  );

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
              onSinceChange(event.currentTarget.value ? new Date(event.currentTarget.value) : null)
            }
          />
          <TextField
            label="Do"
            name="scoreboard-until"
            type="date"
            value={until?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onUntilChange(event.currentTarget.value ? new Date(event.currentTarget.value) : null)
            }
          />
        </>
      ) : (
        <TextField
          label="Referenční datum"
          name="scoreboard-reference-date"
          type="date"
          value={referenceDate.toISOString().slice(0, 10)}
          onChange={(event) =>
            onReferenceDateChange(
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
