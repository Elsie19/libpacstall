use std::env;

/// Get number of CPUs or fetch from `$PACSTALL_BUILD_CORES`.
pub fn ncpu() -> usize {
    let cpu_count = num_cpus::get();
    if let Ok(cpu) = env::var("PACSTALL_BUILD_CORES") {
        cpu.parse::<usize>().unwrap_or(cpu_count)
    } else {
        cpu_count
    }
}
