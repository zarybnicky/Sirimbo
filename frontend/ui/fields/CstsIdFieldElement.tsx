import { CstsPersonDocument } from '@/graphql/Person';
import { CstsPersonSourceLink } from '@/ui/CstsPersonSourceLink';
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
      {query.data ? (
        query.data.cstsAthlete ? (
          <span className="inline-flex items-center gap-1 text-green-9">
            <span>{query.data.cstsAthlete}</span>
            <CstsPersonSourceLink
              idt={idt}
              className="text-green-9 hover:text-green-11"
            />
          </span>
        ) : (
          <span className="text-accent-9">Nenalezeno</span>
        )
      ) : null}
    </div>
  );
}
