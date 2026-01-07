import { CstsPersonDocument } from '@/graphql/Person';
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
  ...props
}: CstsIdFieldElementProps<T>) {
  const value = useWatch({ control: control!, name }) as string | undefined;

  const idt = React.useMemo(() => {
    if (!value) {
      return Number.NaN;
    }

    const parsed = Number.parseInt(value, 10);
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
        {...props}
      />
      {query.data ? (
        query.data.cstsAthlete ? (
          <span className="text-green-9">{query.data.cstsAthlete}</span>
        ) : (
          <span className="text-accent-9">Nenalezeno</span>
        )
      ) : null}
    </div>
  );
}
