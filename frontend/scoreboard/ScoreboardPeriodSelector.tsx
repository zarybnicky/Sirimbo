import React from 'react';
import { Combobox } from '@/ui/fields/Combobox';
import { TextField } from '@/ui/fields/text';
import { PeriodPreset, periodLabels } from './periods';

type ScoreboardPeriodSelectorProps = {
  preset: PeriodPreset;
  onPresetChange: (preset: PeriodPreset) => void;
  referenceDate: Date;
  onReferenceDateChange: (value: Date) => void;
  customSince: Date | null;
  onCustomSinceChange: (value: Date | null) => void;
  customUntil: Date | null;
  onCustomUntilChange: (value: Date | null) => void;
  showCustomRangeWarning: boolean;
};

export function ScoreboardPeriodSelector({
  preset,
  onPresetChange,
  referenceDate,
  onReferenceDateChange,
  customSince,
  onCustomSinceChange,
  customUntil,
  onCustomUntilChange,
  showCustomRangeWarning,
}: ScoreboardPeriodSelectorProps) {
  const periodOptions = React.useMemo(
    () =>
      Object.entries(periodLabels).map(([value, label]) => ({
        id: value,
        label,
      })),
    [],
  );

  const handlePresetChange = React.useCallback<
    React.Dispatch<React.SetStateAction<string | null | undefined>>
  >(
    (value) => {
      const nextValue = typeof value === 'function' ? value(preset) : value;
      if (typeof nextValue === 'string') {
        onPresetChange(nextValue as PeriodPreset);
      }
    },
    [onPresetChange, preset],
  );

  return (
    <>
      <Combobox
        label="Období"
        value={preset}
        onChange={handlePresetChange}
        options={periodOptions}
        placeholder="Vyberte období"
      />

      {preset === 'custom' ? (
        <>
          <TextField
            label="Od"
            name="scoreboard-custom-since"
            type="date"
            value={customSince?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onCustomSinceChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
          <TextField
            label="Do"
            name="scoreboard-custom-until"
            type="date"
            value={customUntil?.toISOString()?.slice(0, 10)}
            onChange={(event) =>
              onCustomUntilChange(
                event.currentTarget.value ? new Date(event.currentTarget.value) : null,
              )
            }
          />
          {showCustomRangeWarning ? (
            <p className="text-sm text-neutral-10 md:col-span-2 xl:col-span-4">
              Vyplňte prosím datum od i do, aby bylo možné žebříček spočítat.
            </p>
          ) : null}
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
