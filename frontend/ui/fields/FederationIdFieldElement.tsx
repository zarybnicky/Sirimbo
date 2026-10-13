import { FederationPersonLookupDocument } from '@/graphql/Federation';
import { FederationLink } from '@/ui/federation-links';
import { TextFieldElement, type TextFieldElementProps } from '@/ui/fields/text';
import React from 'react';
import { type FieldValues, useWatch } from 'react-hook-form';
import { useQuery } from 'urql';

export function FederationIdFieldElement<T extends FieldValues>({
  control,
  name,
  federation,
  placeholder = '10000000',
  ...props
}: TextFieldElementProps<T> & { federation: string }) {
  const value = useWatch({ control, name });
  const id = Number(value);
  const externalId = Number.isSafeInteger(id) && id > 0 ? String(id) : '';
  const [lookupId, setLookupId] = React.useState(externalId);

  React.useEffect(() => {
    const timeout = setTimeout(() => setLookupId(externalId), 300);
    return () => clearTimeout(timeout);
  }, [externalId]);

  const [query] = useQuery({
    query: FederationPersonLookupDocument,
    pause: !lookupId || lookupId !== externalId,
    variables: { federation, externalId: lookupId || '0' },
  });
  const person = query.data?.federationPersonByFederationAndExternalId;
  let helperText = props.helperText;
  if (externalId && externalId === lookupId && !query.fetching && !query.stale) {
    if (query.error) {
      helperText = 'Vyhledávání se nezdařilo';
    } else if (query.data) {
      helperText = person ? (
        <FederationLink
          entity="people"
          federation={federation}
          externalId={person.externalId}
        >
          {person.canonicalName ||
            [person.firstName, person.lastName].filter(Boolean).join(' ') ||
            person.externalId}
        </FederationLink>
      ) : (
        'Nenalezeno'
      );
    }
  }

  return (
    <TextFieldElement
      {...props}
      control={control}
      name={name}
      placeholder={placeholder}
      type="number"
      helperText={helperText}
    />
  );
}
