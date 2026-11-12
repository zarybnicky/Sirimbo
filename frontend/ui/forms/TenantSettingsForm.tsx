import {
  ReplaceTenantSettingsDocument,
  TenantSettingsDocument,
} from '@/graphql/Tenant';
import { DialogTitle } from '@/ui/dialog';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  settings: z.string().refine((value) => {
    try {
      const parsed = JSON.parse(value);
      return parsed !== null && typeof parsed === 'object' && !Array.isArray(parsed);
    } catch {
      return false;
    }
  }, 'Nastavení musí být JSON objekt.'),
});

export function TenantSettingsForm({ tenantId }: { tenantId: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const [query] = useQuery({
    query: TenantSettingsDocument,
    variables: { tenantId },
  });
  const [result, replaceSettings] = useMutation(ReplaceTenantSettingsDocument);
  const settings = query.data?.tenantSetting;

  React.useEffect(() => {
    if (!settings) return;
    reset(
      { settings: JSON.stringify(settings.settings ?? {}, null, 2) },
      { keepDirtyValues: true, keepTouched: true, keepErrors: true },
    );
  }, [reset, settings]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!settings || query.error) return;
    const saved = await replaceSettings({
      tenantId,
      settings: JSON.parse(values.settings),
    });
    if (!saved.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Upravit nastavení klubu</DialogTitle>
      <FormError error={result.error ?? query.error} />
      <TextAreaElement
        control={control}
        name="settings"
        label="Nastavení (JSON)"
        className="font-mono"
      />
      <SubmitButton control={control} disabled={!settings || !!query.error} />
    </form>
  );
}
