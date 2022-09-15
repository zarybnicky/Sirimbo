import { useConfig } from "lib/use-config";
import { NewLayout } from "components/NewLayout";
import { OldLayout } from "components/OldLayout";

export const Layout: React.FC = ({ children }) => {
  const { layout } = useConfig();
  const Layout = layout === 'new' ? NewLayout : OldLayout;
  return <Layout>{children}</Layout>;
};
