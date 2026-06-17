import { CurrentTenantDocument } from '@/graphql/Tenant';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import React from 'react';
import { type Control, useController, useWatch } from 'react-hook-form';
import { useQuery } from 'urql';
import { z } from 'zod';
import { EventForm, type EventFormType } from './types';

export function eventLocationInput({
  locationId,
  locationText,
}: Pick<EventFormType, 'locationId' | 'locationText'>) {
  return {
    locationId:
      locationId && locationId !== 'none' && locationId !== 'other' ? locationId : null,
    locationText: locationId === 'none' ? '' : (locationText ?? ''),
  };
}

export function LocationField({
  control,
}: {
  control: Control<z.input<typeof EventForm>, unknown, z.infer<typeof EventForm>>;
}) {
  const locationId = useWatch({ control, name: 'locationId' });
  const { field: locationText } = useController({ control, name: 'locationText' });
  const { onChange: setLocationText, value: locationTextValue } = locationText;
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const locationOptions = React.useMemo(
    () => [
      { id: 'none', label: 'Žádné' },
      ...(tenant?.tenant?.tenantLocationsList || []).map((location) => ({
        id: location.id,
        label: location.name,
      })),
      { id: 'other', label: 'Jiné...' },
    ],
    [tenant],
  );

  React.useEffect(() => {
    if (locationId !== 'other' && locationTextValue) {
      setLocationText('');
    }
  }, [locationId, locationTextValue, setLocationText]);

  return (
    <>
      <RadioButtonGroupElement
        control={control}
        name="locationId"
        options={locationOptions}
        label="Místo konání"
      />
      {locationId === 'other' && (
        <TextFieldElement
          control={control}
          name="locationText"
          placeholder="Místo konání"
        />
      )}
    </>
  );
}
