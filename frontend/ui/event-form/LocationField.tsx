import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { FieldHelper } from '@/ui/form';
import { buttonCls } from '@/ui/style';
import React from 'react';
import { useController } from 'react-hook-form';
import type { EventFormControl } from './types';

export function LocationField({ control, locations }: {
  control: EventFormControl;
  locations: { id: string; name: string }[];
}) {
  const { field, fieldState } = useController({ control, name: 'locationId' });
  const locationId = field.value;
  const { field: locationText } = useController({ control, name: 'locationText' });
  const { onChange: setLocationText, value: locationTextValue } = locationText;
  const [editing, setEditing] = React.useState(!locationId || locationId === 'none');
  const locationOptions = React.useMemo(
    () => [
      { id: 'none', label: 'Žádné' },
      ...locations.map(({ id, name }) => ({ id, label: name })),
      { id: 'other', label: 'Jiné...' },
    ],
    [locations],
  );

  React.useEffect(() => {
    if (locationId !== 'other' && locationTextValue) {
      setLocationText('');
    }
  }, [locationId, locationTextValue, setLocationText]);

  return (
    <div className="relative">
      <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
        <b>Místo konání</b>
        {!editing && (
          <button
            type="button"
            className={buttonCls({ variant: 'outline', size: 'xs' })}
            onClick={() => setEditing(true)}
            aria-label="Změnit místo konání"
          >
            {locationId === 'other'
              ? locationTextValue
              : (locationOptions.find((x) => x.id === locationId)?.label ?? 'Načítám…')}
          </button>
        )}
      </div>
      {editing && (
        <>
          <RadioButtonGroup
            layout="wrap"
            name={field.name}
            value={locationId ?? 'none'}
            options={locationOptions}
            onBlur={field.onBlur}
            onValueChange={(id) => {
              field.onChange(id);
              if (id !== 'other') setEditing(false);
            }}
          />
          {locationId === 'other' && (
            <div className="flex items-center gap-2">
              <TextFieldElement
                control={control}
                name="locationText"
                placeholder="Místo konání"
              />
              <button
                type="button"
                className={buttonCls({ variant: 'outline', size: 'sm' })}
                disabled={!locationTextValue?.trim()}
                onClick={() => setEditing(false)}
              >
                Použít
              </button>
            </div>
          )}
        </>
      )}
      <FieldHelper error={fieldState.error} />
    </div>
  );
}
