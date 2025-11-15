use crate::chaos::invariants::Invariant;
use crate::chaos::trace::{BugPattern, TraceEntry, TraceEvent};
use std::time::Duration;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TestReport{
    pub is_success: bool,
    pub seed: u64,
    pub total_duration: Duration,
    pub total_events: usize,
    pub violations: Vec<String>,
    pub bugs: Vec<BugPattern>,
    pub summary_by_event: HashMap<String, usize>,
    pub trace: Vec<TraceEntry>
}

impl TestReport {
    pub fn new(seed: u64, violations: Vec<String>, bugs: Vec<BugPattern>, trace: Vec<TraceEntry>) -> Self {
        let is_success = violations.is_empty() && bugs.is_empty();
        let total_events = trace.len();

        let total_duration = if let (Some(first), Some(last)) = (trace.first(), trace.last()) {
            last.timestamp.duration_since(first.timestamp)
        }
        else {
            Duration::ZERO
        };

        let mut summary_by_event = HashMap::new();
        for entry in &trace {
            let event_name = match &entry.event {
                TraceEvent::ChaosInjected { operation, .. } => format!("Chaos:{}", operation),
                TraceEvent::Partition { .. } => "Partition".to_string(),
                TraceEvent::PartitionHealed { .. } => "PartitionHealed".to_string(),
                TraceEvent::PacketDropped { .. } => "PacketDropped".to_string(),
                TraceEvent::DelayInjected { .. } => "DelayInjected".to_string(),
                TraceEvent::ConnectionAttempt { .. } => "ConnectionAttempt".to_string(),
                TraceEvent::ConnectionSuccess { .. } => "ConnectionSuccess".to_string(),
                TraceEvent::ConnectionFailed { .. } => "ConnectionFailed".to_string(),
                TraceEvent::UserMarker { label, .. } => format!("Marker:{}", label),
                _ => "Other".to_string(),
            };
            *summary_by_event.entry(event_name).or_insert(0) += 1;
        }

        Self {
            is_success,
            seed,
            total_duration,
            total_events,
            violations,
            bugs,
            summary_by_event,
            trace
        }
    }

    pub fn generate_report_string(&self) -> String {
        let mut report = String::new();
        let status_icon = if self.is_success { "✅" } else { "❌" };
        let status_text = if self.is_success { "PASSED" } else { "FAILED" };

        report.push_str("╔═════════════════════════════════════════════════════════════╗\n");
        report.push_str(&format!(
            "║                  Fracture Test Report                     ║\n"
        ));
        report.push_str("╠═════════════════════════════════════════════════════════════╣\n");
        report.push_str(&format!(
            "║ Status:   {: <48} ║\n",
            format!("{} {}", status_icon, status_text)
        ));
        report.push_str(&format!("║ Seed:     {: <48} ║\n", self.seed));
        report.push_str(&format!(
            "║ Duration: {: <48} ║\n",
            format!("{:?}", self.total_duration)
        ));
        report.push_str(&format!(
            "║ Events:   {: <48} ║\n",
            self.total_events
        ));
        report.push_str("╠═════════════════════════════════════════════════════════════╣\n");

        if !self.is_success {
            if !self.violations.is_empty() {
                report.push_str("║ 🚨 Invariant Violations:                                    ║\n");
                for (i, violation) in self.violations.iter().enumerate() {
                    report.push_str(&format!(
                        "║   {}. {: <51} ║\n",
                        i + 1,
                        violation
                    ));
                }
                report.push_str("║                                                             ║\n");
            }

            if !self.bugs.is_empty() {
                report.push_str("║ 🐛 Detected Bug Patterns:                                     ║\n");
                for (i, bug) in self.bugs.iter().enumerate() {
                    let desc = bug.description();
                    let severity = if bug.is_critical() {
                        "(Critical)"
                    } else {
                        ""
                    };
                    report.push_str(&format!(
                        "║   {}. {: <49} ║\n",
                        i + 1,
                        format!("{} {}", desc, severity)
                    ));
                }
                report.push_str("║                                                             ║\n");
            }
        } else {
            report.push_str("║ All invariants passed. No bug patterns detected.            ║\n");
        }

        report.push_str("╠═════════════════════════════════════════════════════════════╣\n");
        report.push_str("║ Event Summary:                                              ║\n");
        let mut sorted_summary: Vec<_> = self.summary_by_event.iter().collect();
        sorted_summary.sort_by_key(|(k, _)| *k);

        for (event, count) in sorted_summary {
            report.push_str(&format!(
                "║   - {: <30}: {: <20} ║\n",
                event, count
            ));
        }

        report.push_str("╚═════════════════════════════════════════════════════════════╝\n");

        if !self.is_success {
            report.push_str("\n\nChaos Trace (Last 20 events):\n");
            let trace_log = self.trace.iter().rev().take(20).rev()
                .map(|entry| format!("  - {:?} at {:?}", entry.event, entry.timestamp))
                .collect::<Vec<String>>()
                .join("\n");
            report.push_str(&trace_log);
            report.push_str(&format!(
                "\n\nRun with FRACTURE_SEED={} to reproduce.",
                self.seed
            ));
        }

        report
    }
}

pub fn generate_report(seed: u64, violations: Vec<String>, bugs: Vec<BugPattern>, trace: Vec<TraceEntry>) -> TestReport {
    TestReport::new(seed, violations, bugs, trace)
}