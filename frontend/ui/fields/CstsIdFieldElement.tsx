import { CstsPersonDocument } from '@/graphql/Person';
import { CstsPersonLink } from '@/ui/csts-links';
import { TextFieldElement, type TextFieldElementProps } from '@/ui/fields/text';
import React from 'react';
import { type FieldValues, useWatch } from 'react-hook-form';
import { useQuery } from 'urql';

type CstsIdFieldElementProps<T extends FieldValues> = TextFieldElementProps<T> & {
  wrapperClassName?: string;
};

export function CstsIdFieldElement<T extends FieldValues>({
  control,
  name,
  label = 'ČSTS IDT',
  placeholder = '10000000',
  wrapperClassName,
  type = 'number',
  ...props
}: CstsIdFieldElementProps<T>) {
  const value = useWatch({ control: control!, name }) as number | string | undefined;

  const idt = React.useMemo(() => {
    if (value === null || value === undefined || value === '') {
      return Number.NaN;
    }

    const parsed = typeof value === 'number' ? value : Number.parseInt(value, 10);
    return Number.isNaN(parsed) ? Number.NaN : parsed;
  }, [value]);

  const [query] = useQuery({
    query: CstsPersonDocument,
    pause: Number.isNaN(idt),
    variables: { idt },
  });

  return (
    <div className={wrapperClassName}>
      <TextFieldElement
        control={control}
        name={name}
        label={label}
        placeholder={placeholder}
        type={type}
        {...props}
      />
      {!query.data ? null : !query.data.cstsAthlete ? (
        <span className="text-accent-9">Nenalezeno</span>
      ) : (
        <CstsPersonLink idt={idt} className="text-green-9 hover:text-green-11">
          {query.data.cstsAthlete}
        </CstsPersonLink>
      )}
    </div>
  );
}
