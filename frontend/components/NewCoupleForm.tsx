import { useUserListQuery } from 'lib/graphql/User';
import { useCreateCoupleMutation } from 'lib/graphql/Couple';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = {
  man: string;
  woman: string;
};

export const NewCoupleForm: React.FC<{
  onSuccess?: () => void;
}> = ({ onSuccess }) => {
  const { data: users } = useUserListQuery();
  const men = React.useMemo(
    () =>
      (users?.users?.nodes || [])
        .filter((x) => x.uPohlavi === 'm')
        .map((x) => ({ id: x.id, label: `${x.uJmeno} ${x.uPrijmeni} (${x.id})` })),
    [users],
  );
  const women = React.useMemo(
    () =>
      (users?.users?.nodes || [])
        .filter((x) => x.uPohlavi === 'f')
        .map((x) => ({ id: x.id, label: `${x.uJmeno} ${x.uPrijmeni} (${x.id})` })),
    [users],
  );

  const { mutateAsync: doCreate } = useCreateCoupleMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>();
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doCreate({ man: values.man, woman: values.woman });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <SelectElement
        control={control}
        name="man"
        label="Partner"
        required
        options={men}
      />
      <SelectElement
        control={control}
        name="woman"
        label="Partnerka"
        required
        options={women}
      />
      <SubmitButton className="w-full" loading={onSubmit.loading}>
        Spárovat
      </SubmitButton>
    </form>
  );
};
