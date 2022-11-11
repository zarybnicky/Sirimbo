import { Container } from "@mui/material";
import { CohortForm } from "components/CohortForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function CohortAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <CohortForm onSuccess={() => router.back()} />
  </div>;
};
