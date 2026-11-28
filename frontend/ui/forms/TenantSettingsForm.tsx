import {
  ReplaceTenantSettingsDocument,
  TenantSettingsDocument,
} from '@/graphql/Tenant';
import { DialogTitle } from '@/ui/dialog';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  settings: z.string().transform((value, ctx) => {
    try {
      const parsed: unknown = JSON.parse(value);
      if (parsed !== null && typeof parsed === 'object' && !Array.isArray(parsed)) {
        return parsed as Record<string, unknown>;
      }
    } catch {
      // Report invalid JSON and non-object values with the same message below.
    }
    ctx.addIssue({ code: 'custom', message: 'Nastavení musí být JSON objekt.' });
    return z.NEVER;
  }),
});

export function TenantSettingsForm({ tenantId }: { tenantId: string }) {
  const { onSuccess } = useFormResult();
  const [query] = useQuery({
    query: TenantSettingsDocument,
    variables: { tenantId },
  });
  const [result, replaceSettings] = useMutation(ReplaceTenantSettingsDocument);
  const settings = query.data?.tenantSetting;
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    values: settings
      ? { settings: JSON.stringify(settings.settings ?? {}, null, 2) }
      : undefined,
    resetOptions: { keepDirtyValues: true, keepTouched: true, keepErrors: true },
  });

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!settings || query.error) return;
    const saved = await replaceSettings({
      tenantId,
      settings: values.settings,
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
